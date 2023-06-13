package co.topl.blockchain

import BlockchainPeerHandler.monoidBlockchainPeerHandler
import cats.Parallel
import cats.data.OptionT
import cats.data.Validated
import cats.effect._
import cats.effect.std.Random
import cats.implicits._
import co.topl.algebras._
import co.topl.blockchain.algebras.EpochDataAlgebra
import co.topl.blockchain.interpreters.BlockchainPeerServer
import co.topl.brambl.validation._
import co.topl.catsutils.DroppingTopic
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.algebras._
import co.topl.consensus.models.BlockId
import co.topl.consensus.models.SlotData
import co.topl.crypto.signing.Ed25519VRF
import co.topl.eventtree.EventSourcedState
import co.topl.eventtree.ParentChildTree
import co.topl.grpc.NodeGrpc
import co.topl.grpc.ToplGrpc
import co.topl.ledger.algebras._
import co.topl.ledger.interpreters.TransactionRewardCalculator
import co.topl.minting.algebras.StakingAlgebra
import co.topl.minting.interpreters._
import co.topl.networking.blockchain._
import co.topl.networking.fsnetwork.ActorPeerHandlerBridgeAlgebra
import co.topl.networking.p2p._
import co.topl.typeclasses.implicits._
import fs2.{io => _, _}
import io.grpc.ServerServiceDefinition
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats._
import scala.jdk.CollectionConverters._

object Blockchain {

  /**
   * A program which executes the blockchain protocol, including a P2P layer, RPC layer, and minter.
   */
  def make[F[_]: Parallel: Async: Random](
    clock:                     ClockAlgebra[F],
    stakerOpt:                 Option[StakingAlgebra[F]],
    dataStores:                DataStores[F],
    _localChain:               LocalChainAlgebra[F],
    chainSelectionAlgebra:     ChainSelectionAlgebra[F, SlotData],
    blockIdTree:               ParentChildTree[F, BlockId],
    blockHeights:              EventSourcedState[F, Long => F[Option[BlockId]], BlockId],
    validators:                Validators[F],
    _mempool:                  MempoolAlgebra[F],
    ed25519VrfResource:        UnsafeResource[F, Ed25519VRF],
    localPeer:                 LocalPeer,
    remotePeers:               Stream[F, DisconnectedPeer],
    rpcHost:                   String,
    rpcPort:                   Int,
    nodeProtocolConfiguration: ProtocolConfigurationAlgebra[F, Stream[F, *]],
    additionalGrpcServices:    List[ServerServiceDefinition],
    experimentalP2P:           Boolean = false,
    _epochData:                EpochDataAlgebra[F]
  ): Resource[F, Unit] = {
    implicit val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("Bifrost.Blockchain")
    for {
      (localChain, blockAdoptionsTopic)    <- LocalChainBroadcaster.make(_localChain)
      (mempool, transactionAdoptionsTopic) <- MempoolBroadcaster.make(_mempool)
      // Whenever a block is adopted locally, broadcast all of its corresponding _transactions_ to eagerly notify peers
      _ <- Async[F].background(
        blockAdoptionsTopic.subscribeUnbounded
          .evalMap(id => dataStores.bodies.getOrRaise(id))
          .flatMap(b => Stream.iterable(b.transactionIds))
          .through(transactionAdoptionsTopic.publish)
          .compile
          .drain
      )
      synchronizationHandler <-
        if (experimentalP2P) {
          ActorPeerHandlerBridgeAlgebra.make(
            localChain,
            chainSelectionAlgebra,
            validators.header,
            validators.headerToBody,
            validators.bodySyntax,
            validators.bodySemantics,
            validators.bodyAuthorization,
            dataStores.slotData,
            dataStores.headers,
            dataStores.bodies,
            dataStores.transactions,
            blockIdTree
          )
        } else {
          Resource.pure[F, BlockchainPeerHandlerAlgebra[F]](
            BlockchainPeerHandler.ChainSynchronizer.make[F](
              clock,
              localChain,
              validators.header,
              validators.headerToBody,
              validators.bodySyntax,
              validators.bodySemantics,
              validators.bodyAuthorization,
              dataStores.slotData,
              dataStores.headers,
              dataStores.bodies,
              dataStores.transactions,
              blockIdTree
            )
          )
        }
      clientHandler <- Resource.pure[F, BlockchainPeerHandlerAlgebra[F]](
        List(
          synchronizationHandler,
          BlockchainPeerHandler.FetchMempool.make(
            validators.transactionSyntax,
            dataStores.transactions,
            mempool
          ),
          BlockchainPeerHandler.CommonAncestorSearch.make(
            id =>
              OptionT(
                localChain.head
                  .map(_.slotId.blockId)
                  .flatMap(blockHeights.useStateAt(_)(_.apply(id)))
              ).toRight(new IllegalStateException("Unable to determine block height tree")).rethrowT,
            () => localChain.head.map(_.height),
            dataStores.slotData
          )
        ).combineAll
      )
      peerServerF = BlockchainPeerServer.make(
        dataStores.slotData.get,
        dataStores.headers.get,
        dataStores.bodies.get,
        dataStores.transactions.get,
        blockHeights,
        localChain,
        mempool,
        blockAdoptionsTopic,
        transactionAdoptionsTopic
      ) _
      _ <- BlockchainNetwork
        .make[F](
          localPeer.localAddress.host,
          localPeer.localAddress.port,
          localPeer,
          remotePeers,
          clientHandler,
          peerServerF
        )

      droppingBlockAdoptionsTopic <- DroppingTopic(blockAdoptionsTopic, 10)
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
          droppingBlockAdoptionsTopic.subscribeUnbounded,
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
          staker           <- Stream.fromOption[F](stakerOpt)
          rewardCalculator <- Stream.resource(TransactionRewardCalculator.make[F])
          costCalculator = TransactionCostCalculatorInterpreter.make[F](TransactionCostConfig())
          blockPacker <- Stream.resource(
            BlockPacker
              .make[F](
                mempool,
                validators.boxState,
                rewardCalculator,
                costCalculator,
                validators.transactionAuthorization
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
                      .resource(DroppingTopic(blockAdoptionsTopic, 1))
                      .flatMap(_.subscribeUnbounded)
                      .evalMap(dataStores.slotData.getOrRaise)
                  ),
                staker,
                clock,
                blockPacker
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
            dataStores.bodies.put(id, block.body) &>
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
