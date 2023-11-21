package co.topl.blockchain

import cats.data._
import cats.effect._
import cats.effect.std.{Queue, Random}
import cats.implicits._
import cats.effect.implicits._
import co.topl.algebras._
import co.topl.blockchain.algebras.EpochDataAlgebra
import co.topl.blockchain.interpreters.BlockchainPeerServer
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
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
import co.topl.ledger.interpreters.QuivrContext
import co.topl.ledger.models.StaticBodyValidationContext
import co.topl.minting.algebras.StakingAlgebra
import co.topl.minting.interpreters._
import co.topl.networking.blockchain._
import co.topl.networking.fsnetwork.DnsResolverInstances.DefaultDnsResolver
import co.topl.networking.fsnetwork.ReverseDnsResolverInstances.{DefaultReverseDnsResolver, NoOpReverseResolver}
import co.topl.networking.fsnetwork._
import co.topl.networking.p2p._
import co.topl.node.models.{Block, BlockBody, FullBlock}
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
    rpcHost:                   String,
    rpcPort:                   Int,
    nodeProtocolConfiguration: ProtocolConfigurationAlgebra[F, Stream[F, *]],
    additionalGrpcServices:    List[ServerServiceDefinition],
    _epochData:                EpochDataAlgebra[F],
    exposeServerPort:          Boolean,
    networkProperties:         NetworkProperties
  ): Resource[F, Unit] = new BlockchainImpl[F](
    clock,
    stakerResource,
    dataStores,
    localChain,
    chainSelectionAlgebra,
    blockIdTree,
    blockHeights,
    validators,
    _mempool,
    ed25519VrfResource,
    localPeer,
    knownPeers,
    rpcHost,
    rpcPort,
    nodeProtocolConfiguration,
    additionalGrpcServices,
    _epochData,
    exposeServerPort,
    networkProperties
  ).resource

}

class BlockchainImpl[F[_]: Async: Random: Dns](
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
  rpcHost:                   String,
  rpcPort:                   Int,
  nodeProtocolConfiguration: ProtocolConfigurationAlgebra[F, Stream[F, *]],
  additionalGrpcServices:    List[ServerServiceDefinition],
  _epochData:                EpochDataAlgebra[F],
  exposeServerPort:          Boolean,
  networkProperties:         NetworkProperties
) {
  implicit private val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("Bifrost.Blockchain")

  /**
   * Whenever a block is adopted locally, broadcast all of its corresponding (non-reward) _transactions_ to eagerly notify peers
   */
  private def adoptedBlockTxRebroadcaster(transactionsTopic: Topic[F, TransactionId]) =
    Resource.make(Logger[F].info("Initializing Block-Tx Rebroadcaster"))(_ =>
      Logger[F].info("Block-Tx Rebroadcaster Terminated")
    ) >>
    Stream
      .force(localChain.adoptions)
      .evalMap(id => dataStores.bodies.getOrRaise(id))
      .flatMap(b => Stream.iterable(b.transactionIds))
      .through(transactionsTopic.publish)
      .compile
      .drain
      .background

  private def p2p(mempool: MempoolAlgebra[F], transactionsTopic: Topic[F, TransactionId]) =
    for {
      _           <- Resource.make(Logger[F].info("Initializing P2P"))(_ => Logger[F].info("P2P Terminated"))
      remotePeers <- Queue.unbounded[F, DisconnectedPeer].toResource
      peersStatusChangesTopic <- Resource.make(Topic[F, PeerConnectionChange])(_.close.void)
      _                       <- Logger[F].info(s"Received known peers from config: $knownPeers").toResource
      currentPeers            <- Ref.of[F, Set[RemoteAddress]](Set.empty[RemoteAddress]).toResource
      initialPeers = knownPeers.map(kp => DisconnectedPeer(RemoteAddress(kp.host, kp.port), (0, 0)))
      remotePeersStream = Stream.fromQueueUnterminated[F, DisconnectedPeer](remotePeers)
      implicit0(dnsResolver: DnsResolver[F]) = new DefaultDnsResolver[F]()
      implicit0(reverseDnsResolver: ReverseDnsResolver[F]) =
        if (networkProperties.useHostNames) new DefaultReverseDnsResolver[F]() else new NoOpReverseResolver[F]
      bridge <- ActorPeerHandlerBridgeAlgebra
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
          initialPeers,
          peersStatusChangesTopic,
          remotePeers.offer,
          currentPeers.set
        )
        .onFinalize(Logger[F].info("P2P Actor system had been shutdown"))
      p2pBlockAdoptionsTopic <- Resource.make(Topic[F, BlockId])(_.close.void)
      _ <- Stream.force(localChain.adoptions).through(p2pBlockAdoptionsTopic.publish).compile.drain.background
      _ <- Logger[F].info(s"Exposing server port is ${if (exposeServerPort) "enabled" else "disabled"}").toResource
      peerServerF = BlockchainPeerServer.make(
        dataStores.slotData.get,
        dataStores.headers.get,
        dataStores.bodies.get,
        dataStores.transactions.get,
        blockHeights,
        if (exposeServerPort) () => Option(localPeer.localAddress.port) else () => None,
        () => currentPeers.get,
        localChain,
        mempool,
        p2pBlockAdoptionsTopic,
        transactionsTopic,
        peersStatusChangesTopic
      ) _
      _ <- BlockchainNetwork
        .make[F](
          localPeer.localAddress.host,
          localPeer.localAddress.port,
          localPeer,
          remotePeersStream,
          bridge,
          peerServerF,
          peersStatusChangesTopic
        )
    } yield ()

  def rpc(mempool: MempoolAlgebra[F]): Resource[F, Unit] =
    for {
      _ <- Resource.make(Logger[F].info("Initializing RPC"))(_ => Logger[F].info("RPC Terminated"))
      rpcInterpreter <- ToplRpcServer
        .make(
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
        .toResource
      nodeGrpcService <- NodeGrpc.Server.service[F](rpcInterpreter)
      rpcServer <- ToplGrpc.Server
        .serve(rpcHost, rpcPort)(nodeGrpcService :: additionalGrpcServices)
      _ <- Logger[F].info(s"RPC Server bound at ${rpcServer.getListenSockets.asScala.toList.mkString(",")}").toResource
    } yield ()

  def blockProduction(mempool: MempoolAlgebra[F]): Resource[F, Unit] =
    for {
      _ <- Resource.make(Logger[F].info("Initializing local blocks (potential no-op)"))(_ =>
        Logger[F].info("Local blocks terminated")
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
          // The BlockProducer needs a stream/Source of "parents" upon which it should build.  This stream is the
          // concatenation of the current local head with the stream of local block adoptions
          parentBlocksStream = Stream
            .eval(Sync[F].defer(localChain.head))
            .evalTap(head => clock.delayedUntilSlot(head.slotId.slot))
            .append(
              Stream
                .force(localChain.adoptions)
                .dropOldest(1)
                .evalMap(dataStores.slotData.getOrRaise)
            )
          blockProducer <- Stream.eval(
            BlockProducer.make[F](parentBlocksStream, staker, clock, blockPacker, validators.rewardCalculator)
          )
          block <- Stream.force(blockProducer.blocks)
        } yield block
      _ <- mintedBlockStream
        .evalTap { block =>
          val id = block.header.id
          blockIdTree.associate(id, block.header.parentHeaderId) &>
          dataStores.headers.put(id, block.header) &>
          dataStores.bodies
            .put(id, BlockBody(block.fullBody.transactions.map(_.id), block.fullBody.rewardTransaction.map(_.id))) &>
          block.fullBody.rewardTransaction.traverse(tx => dataStores.transactions.put(tx.id, tx))
        }
        // Validate the local block.  If invalid, skip it "gracefully"
        .evalFilter(validateLocalBlock(_).toOption.isDefined)
        .evalMap(block =>
          ed25519VrfResource
            .use(implicit e => Sync[F].delay(block.header.slotData))
            .flatTap(dataStores.slotData.put(block.header.id, _))
        )
        .evalTap(slotData =>
          localChain
            .isWorseThan(slotData)
            .ifM(
              localChain.adopt(Validated.Valid(slotData)),
              Logger[F].warn("Skipping adoption of local block due to better local chain.")
            )
        )
        .compile
        .drain
        .background
    } yield ()

  def resource: Resource[F, Unit] =
    for {
      _ <- Resource.make(Logger[F].info("Initializing Blockchain"))(_ => Logger[F].info("Blockchain Terminated"))
      (mempool, transactionsTopic) <- MempoolBroadcaster.make(_mempool)
      _ <- (
        adoptedBlockTxRebroadcaster(transactionsTopic),
        p2p(mempool, transactionsTopic),
        rpc(mempool),
        blockProduction(mempool)
      ).parTupled
      _ <- Resource.never[F, Unit]
    } yield ()

  /**
   * Performs all header+body validations of the given block.  If invalid, deletes the header and body from storage and
   * logs a warning.
   */
  private def validateLocalBlock(fullBlock: FullBlock) =
    (for {
      _ <- EitherT(validators.header.validate(fullBlock.header)).leftMap(_.show)
      body <- EitherT.liftF(
        Sync[F]
          .delay(
            BlockBody(fullBlock.fullBody.transactions.map(_.id), fullBlock.fullBody.rewardTransaction.map(_.id))
          )
      )
      block = Block(fullBlock.header, body)
      _ <- EitherT(validators.headerToBody.validate(block)).leftMap(_.show)
      _ <- EitherT(validators.bodySyntax.validate(body).map(_.toEither)).leftMap(_.show)
      semanticContext = StaticBodyValidationContext(
        block.header.parentHeaderId,
        block.header.height,
        block.header.slot
      )
      _ <- EitherT(validators.bodySemantics.validate(semanticContext)(body).map(_.toEither)).leftMap(_.show)
      authContext = (tx: IoTransaction) => QuivrContext.forProposedBlock(block.header.height, block.header.slot, tx)
      _ <- EitherT(validators.bodyAuthorization.validate(authContext)(body).map(_.toEither)).leftMap(_.show)
    } yield ())
      .leftSemiflatTap(reason =>
        Logger[F].warn(show"Locally produced block id=${fullBlock.header.id} is invalid. reason=$reason") &>
        dataStores.headers.remove(fullBlock.header.id) &>
        dataStores.bodies.remove(fullBlock.header.id) &>
        fullBlock.fullBody.rewardTransaction.traverseTap(tx => dataStores.transactions.remove(tx.id))
      )
}
