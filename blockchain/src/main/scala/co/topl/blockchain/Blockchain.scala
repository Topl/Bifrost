package co.topl.blockchain

import cats.data.{OptionT, Validated}
import cats.effect._
import cats.implicits._
import cats.Parallel
import co.topl.algebras.{ClockAlgebra, Store, UnsafeResource}
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras._
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.grpc.ToplGrpc
import co.topl.ledger.algebras._
import co.topl.minting.algebras.StakingAlgebra
import co.topl.consensus.models.{BlockHeader, SlotData}
import co.topl.node.models.BlockBody
import co.topl.networking.blockchain._
import co.topl.networking.p2p.{DisconnectedPeer, LocalPeer}
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}
import BlockchainPeerHandler.monoidBlockchainPeerHandler
import cats.effect.std.Random
import co.topl.blockchain.interpreters.BlockchainPeerServer
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.catsutils.DroppingTopic
import co.topl.consensus.models.BlockId
import co.topl.crypto.signing.Ed25519VRF
import co.topl.minting.interpreters.{BlockPacker, BlockProducer}
import co.topl.networking.fsnetwork.ActorPeerHandlerBridgeAlgebra
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.jdk.CollectionConverters._
import fs2._

object Blockchain {

  /**
   * A program which executes the blockchain protocol, including a P2P layer, RPC layer, and minter.
   */
  def make[F[_]: Parallel: Async: Random](
    clock:                       ClockAlgebra[F],
    stakerOpt:                   Option[StakingAlgebra[F]],
    slotDataStore:               Store[F, BlockId, SlotData],
    headerStore:                 Store[F, BlockId, BlockHeader],
    bodyStore:                   Store[F, BlockId, BlockBody],
    transactionStore:            Store[F, TransactionId, IoTransaction],
    _localChain:                 LocalChainAlgebra[F],
    chainSelectionAlgebra:       ChainSelectionAlgebra[F, SlotData],
    blockIdTree:                 ParentChildTree[F, BlockId],
    blockHeights:                EventSourcedState[F, Long => F[Option[BlockId]], BlockId],
    headerValidation:            BlockHeaderValidationAlgebra[F],
    blockHeaderToBodyValidation: BlockHeaderToBodyValidationAlgebra[F],
    transactionSyntaxValidation: TransactionSyntaxVerifier[F],
    bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
    bodySemanticValidation:      BodySemanticValidationAlgebra[F],
    bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F],
    _mempool:                    MempoolAlgebra[F],
    ed25519VrfResource:          UnsafeResource[F, Ed25519VRF],
    localPeer:                   LocalPeer,
    remotePeers:                 Stream[F, DisconnectedPeer],
    rpcHost:                     String,
    rpcPort:                     Int,
    experimentalP2P:             Boolean = false
  ): Resource[F, Unit] = {
    implicit val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F]("Bifrost.Blockchain")
    for {
      (localChain, blockAdoptionsTopic)    <- LocalChainBroadcaster.make(_localChain)
      (mempool, transactionAdoptionsTopic) <- MempoolBroadcaster.make(_mempool)
      // Whenever a block is adopted locally, broadcast all of its corresponding _transactions_ to eagerly notify peers
      _ <- Async[F].background(
        blockAdoptionsTopic.subscribeUnbounded
          .evalMap(id => bodyStore.getOrRaise(id))
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
            headerValidation,
            blockHeaderToBodyValidation,
            bodySyntaxValidation,
            bodySemanticValidation,
            bodyAuthorizationValidation,
            slotDataStore,
            headerStore,
            bodyStore,
            transactionStore,
            blockIdTree
          )
        } else {
          Resource.pure[F, BlockchainPeerHandlerAlgebra[F]](
            BlockchainPeerHandler.ChainSynchronizer.make[F](
              clock,
              localChain,
              headerValidation,
              blockHeaderToBodyValidation,
              bodySyntaxValidation,
              bodySemanticValidation,
              bodyAuthorizationValidation,
              slotDataStore,
              headerStore,
              bodyStore,
              transactionStore,
              blockIdTree
            )
          )
        }
      clientHandler <- Resource.pure[F, BlockchainPeerHandlerAlgebra[F]](
        List(
          synchronizationHandler,
          BlockchainPeerHandler.FetchMempool.make(
            transactionSyntaxValidation,
            transactionStore,
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
            slotDataStore
          )
        ).combineAll
      )
      peerServerF = BlockchainPeerServer.make(
        slotDataStore.get,
        headerStore.get,
        bodyStore.get,
        transactionStore.get,
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
          headerStore,
          bodyStore,
          transactionStore,
          mempool,
          transactionSyntaxValidation,
          localChain,
          blockHeights,
          blockIdTree,
          droppingBlockAdoptionsTopic.subscribeUnbounded
        )
      )
      _ <- ToplGrpc.Server
        .serve(rpcHost, rpcPort, rpcInterpreter)
        .evalTap(rpcServer =>
          Logger[F].info(s"RPC Server bound at ${rpcServer.getListenSockets.asScala.toList.mkString(",")}")
        )
      mintedBlockStream =
        for {
          staker <- Stream.fromOption[F](stakerOpt)
          blockPacker <- Stream.eval(
            BlockPacker
              .make[F](
                mempool,
                transactionStore.getOrRaise,
                transactionStore.contains,
                BlockPacker
                  .makeBodyValidator(bodySyntaxValidation, bodySemanticValidation, bodyAuthorizationValidation)
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
                      .evalMap(slotDataStore.getOrRaise)
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
            headerStore.put(id, block.header) &>
            bodyStore.put(id, block.body) &>
            ed25519VrfResource
              .use(implicit e => Sync[F].delay(block.header.slotData))
              .flatTap(slotDataStore.put(id, _))
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
