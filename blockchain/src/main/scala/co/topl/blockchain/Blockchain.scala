package co.topl.blockchain

import cats.data._
import cats.effect._
import cats.effect.std.{Queue, Random}
import cats.implicits._
import cats.effect.implicits._
import co.topl.algebras._
import co.topl.blockchain.algebras.EpochDataAlgebra
import co.topl.blockchain.interpreters.BlockchainPeerServer
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.brambl.validation._
import co.topl.catsutils.streamAsStreamOps
import co.topl.codecs.bytes.tetra.instances._
import co.topl.config.ApplicationConfig.Bifrost.{KnownPeer, NetworkProperties}
import co.topl.consensus.algebras._
import co.topl.consensus.models._
import co.topl.crypto.signing.Ed25519VRF
import co.topl.eventtree.EventSourcedState
import co.topl.eventtree.ParentChildTree
import co.topl.grpc._
import co.topl.ledger.algebras._
import co.topl.minting.algebras.StakingAlgebra
import co.topl.minting.interpreters._
import co.topl.networking.blockchain._
import co.topl.networking.fsnetwork.{ActorPeerHandlerBridgeAlgebra, DnsResolver, DnsResolverInstances}
import co.topl.networking.p2p._
import co.topl.node.models.BlockBody
import co.topl.typeclasses.implicits._
import com.comcast.ip4s.Dns
import fs2.concurrent.Topic
import fs2.{io => _, _}
import io.grpc.ServerServiceDefinition
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats._

import scala.jdk.CollectionConverters._

object Blockchain {

  /**
   * A program which executes the blockchain protocol, including a P2P layer, RPC layer, and minter.
   */
  def make[F[_]: Async: Random: Dns](
    clock:                     ClockAlgebra[F],
    stakerResource:            Resource[F, Option[StakingAlgebra[F]]],
    dataStores:                DataStores[F],
    localChain:                LocalChainAlgebra[F],
    chainSelectionAlgebra:     ChainSelectionAlgebra[F, SlotData],
    blockIdTree:               ParentChildTree[F, BlockId],
    blockHeights:              EventSourcedState[F, Long => F[Option[BlockId]], BlockId],
    validators:                Validators[F],
    _mempool:                  MempoolAlgebra[F],
    ed25519VrfResource:        Resource[F, Ed25519VRF],
    localPeer:                 LocalPeer,
    knownPeers:                List[KnownPeer],
    knownPeersUnresolvedDns:   List[KnownPeer],
    rpcHost:                   String,
    rpcPort:                   Int,
    nodeProtocolConfiguration: ProtocolConfigurationAlgebra[F, Stream[F, *]],
    additionalGrpcServices:    List[ServerServiceDefinition],
    _epochData:                EpochDataAlgebra[F],
    networkProperties:         NetworkProperties
  ): Resource[F, Unit] = {
    implicit val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("Bifrost.Blockchain")
    implicit val dnsResolver: DnsResolver[F] = DnsResolverInstances.defaultResolver[F]

    for {
      remotePeers             <- Queue.unbounded[F, DisconnectedPeer].toResource
      peersStatusChangesTopic <- Resource.make(Topic[F, PeerConnectionChange])(_.close.void)
      _                       <- Logger[F].info(s"Received known peers from config: $knownPeers").toResource
      currentPeers            <- Ref.of[F, Set[RemoteAddress]](Set.empty[RemoteAddress]).toResource
      initialPeers = knownPeers.map(kp => DisconnectedPeer(RemoteAddress(kp.host, kp.port, kp.resolveDns), (0, 0)))
      initialPeersNoDns = knownPeersUnresolvedDns.map(kp =>
        DisconnectedPeer(RemoteAddress(kp.host, kp.port, kp.resolveDns), (0, 0))
      )
      remotePeersStream <- Resource.pure(Stream.fromQueueUnterminated[F, DisconnectedPeer](remotePeers))
      (mempool, transactionAdoptionsTopic) <- MempoolBroadcaster.make(_mempool)
      // Whenever a block is adopted locally, broadcast all of its corresponding (non-reward) _transactions_ to eagerly notify peers
      _ <- Async[F].background(
        Stream
          .force(localChain.adoptions)
          .evalMap(id => dataStores.bodies.getOrRaise(id))
          .flatMap(b => Stream.iterable(b.transactionIds))
          .through(transactionAdoptionsTopic.publish)
          .compile
          .drain
      )
      synchronizationHandler <- ActorPeerHandlerBridgeAlgebra
        .make(
          localPeer.localAddress.host,
          localChain,
          chainSelectionAlgebra,
          validators.header,
          validators.headerToBody,
          validators.transactionSyntax,
          validators.bodySyntax,
          validators.bodySemantics,
          validators.bodyAuthorization,
          dataStores.slotData,
          dataStores.headers,
          dataStores.bodies,
          dataStores.transactions,
          dataStores.knownHosts,
          blockIdTree,
          blockHeights,
          mempool,
          networkProperties,
          clock,
          initialPeers ::: initialPeersNoDns,
          peersStatusChangesTopic,
          remotePeers.offer,
          currentPeers.set
        )
        .onFinalize(Logger[F].info("P2P Actor system had been shutdown"))

      p2pBlockAdoptionsTopic <- Resource.make(Topic[F, BlockId])(_.close.void)
      _ <- Stream.force(localChain.adoptions).through(p2pBlockAdoptionsTopic.publish).compile.drain.background
      peerServerF = BlockchainPeerServer.make(
        dataStores.slotData.get,
        dataStores.headers.get,
        dataStores.bodies.get,
        dataStores.transactions.get,
        blockHeights,
        () => Option(localPeer.localAddress.port),
        () => currentPeers.get,
        localChain,
        mempool,
        p2pBlockAdoptionsTopic,
        transactionAdoptionsTopic,
        peersStatusChangesTopic
      ) _
      _ <- BlockchainNetwork
        .make[F](
          localPeer.localAddress.host,
          localPeer.localAddress.port,
          localPeer,
          remotePeersStream,
          synchronizationHandler,
          peerServerF,
          peersStatusChangesTopic
        )
      rpcInterpreter <- Resource.eval(
        ToplRpcServer.make(
          dataStores.headers,
          dataStores.bodies,
          dataStores.transactions,
          mempool,
          validators.transactionSyntax,
          localChain,
          blockHeights,
          blockIdTree,
          Stream.force(localChain.adoptions).dropOldest(10),
          nodeProtocolConfiguration,
          _epochData,
          clock
        )
      )
      nodeGrpcService <- NodeGrpc.Server.service[F](rpcInterpreter)
      _ <- ToplGrpc.Server
        .serve(rpcHost, rpcPort)(nodeGrpcService :: additionalGrpcServices)
        .evalTap(rpcServer =>
          Logger[F].info(s"RPC Server bound at ${rpcServer.getListenSockets.asScala.toList.mkString(",")}")
        )
      mintedBlockStream =
        for {
          stakerOpt <- Stream.resource(stakerResource)
          staker    <- Stream.fromOption[F](stakerOpt)
          costCalculator = TransactionCostCalculatorInterpreter.make[F](TransactionCostConfig())
          blockPacker <- Stream.resource(
            BlockPacker
              .make[F](
                mempool,
                validators.boxState,
                validators.rewardCalculator,
                costCalculator,
                validators.transactionAuthorization,
                validators.registrationAccumulator
              )
          )
          blockProducer <- Stream.eval(
            BlockProducer
              .make[F](
                // The BlockProducer needs a stream/Source of "parents" upon which it should build.  This stream is the
                // concatenation of the current local head with the stream of local block adoptions
                Stream
                  .eval(Sync[F].defer(localChain.head))
                  .evalTap(head => clock.delayedUntilSlot(head.slotId.slot))
                  .append(
                    Stream
                      .force(localChain.adoptions)
                      .dropOldest(1)
                      .evalMap(dataStores.slotData.getOrRaise)
                  ),
                staker,
                clock,
                blockPacker,
                validators.rewardCalculator
              )
          )
          block <- Stream.force(blockProducer.blocks)
        } yield block
      _ <- Async[F].background(
        mintedBlockStream
          .evalMap { block =>
            val id = block.header.id
            blockIdTree.associate(id, block.header.parentHeaderId) &>
            dataStores.headers.put(id, block.header) &>
            dataStores.bodies
              .put(id, BlockBody(block.fullBody.transactions.map(_.id), block.fullBody.rewardTransaction.map(_.id))) &>
            block.fullBody.rewardTransaction.traverse(tx => dataStores.transactions.put(tx.id, tx)) &>
            ed25519VrfResource
              .use(implicit e => Sync[F].delay(block.header.slotData))
              .flatTap(dataStores.slotData.put(id, _))
          }
          .map(Validated.Valid(_))
          .evalTap(localChain.adopt)
          .compile
          .drain
      )
      _ <- Resource.never[F, Unit]
    } yield ()
  }

}
