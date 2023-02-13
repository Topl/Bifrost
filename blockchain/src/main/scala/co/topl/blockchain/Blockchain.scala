package co.topl.blockchain

import akka.actor.typed.ActorSystem
import akka.stream.scaladsl.Flow
import akka.util.ByteString
import cats.data.{OptionT, Validated}
import cats.effect._
import cats.implicits._
import cats.Parallel
import co.topl.algebras.{ClockAlgebra, Store, UnsafeResource}
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras.{BlockHeaderValidationAlgebra, LocalChainAlgebra}
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.grpc.ToplGrpc
import co.topl.ledger.algebras._
import co.topl.minting.algebras.StakingAlgebra
import co.topl.{models => legacyModels}
import co.topl.models.utility._
import legacyModels._
import co.topl.consensus.models.{BlockHeader, SlotData}
import co.topl.node.models.BlockBody
import co.topl.networking.blockchain._
import co.topl.networking.p2p.{ConnectedPeer, DisconnectedPeer, LocalPeer}
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}
import BlockchainPeerHandler.monoidBlockchainPeerHandler
import co.topl.blockchain.algebras.BlockHeaderToBodyValidationAlgebra
import co.topl.blockchain.interpreters.BlockchainPeerServer
import co.topl.crypto.signing.Ed25519VRF
import co.topl.minting.interpreters.{BlockPacker, BlockProducer}
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scala.jdk.CollectionConverters._
import scala.util.Random
import fs2._

object Blockchain {

  /**
   * A program which executes the blockchain protocol, including a P2P layer, RPC layer, and minter.
   */
  def make[F[_]: Parallel: Async: FToFuture](
    clock:                       ClockAlgebra[F],
    stakerOpt:                   Option[StakingAlgebra[F]],
    slotDataStore:               Store[F, TypedIdentifier, SlotData],
    headerStore:                 Store[F, TypedIdentifier, BlockHeader],
    bodyStore:                   Store[F, TypedIdentifier, BlockBody],
    transactionStore:            Store[F, TypedIdentifier, Transaction],
    _localChain:                 LocalChainAlgebra[F],
    blockIdTree:                 ParentChildTree[F, TypedIdentifier],
    blockHeights:                EventSourcedState[F, Long => F[Option[TypedIdentifier]], TypedIdentifier],
    headerValidation:            BlockHeaderValidationAlgebra[F],
    blockHeaderToBodyValidation: BlockHeaderToBodyValidationAlgebra[F],
    transactionSyntaxValidation: TransactionSyntaxValidationAlgebra[F],
    bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
    bodySemanticValidation:      BodySemanticValidationAlgebra[F],
    bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F],
    _mempool:                    MempoolAlgebra[F],
    ed25519VrfResource:          UnsafeResource[F, Ed25519VRF],
    localPeer:                   LocalPeer,
    remotePeers:                 Stream[F, DisconnectedPeer],
    peerFlowModifier: (
      ConnectedPeer,
      Flow[ByteString, ByteString, F[BlockchainPeerClient[F]]]
    ) => Flow[ByteString, ByteString, F[BlockchainPeerClient[F]]],
    rpcHost:         String,
    rpcPort:         Int
  )(implicit system: ActorSystem[_], random: Random): Resource[F, Unit] = {
    implicit val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromClass[F](Blockchain.getClass)
    for {
      (localChain, blockAdoptionsTopic)    <- LocalChainBroadcaster.make(_localChain)
      (mempool, transactionAdoptionsTopic) <- MempoolBroadcaster.make(_mempool)
      clientHandler <- Resource.pure[F, BlockchainPeerHandlerAlgebra[F]](
        List(
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
          ),
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
      (_, p2pFiber) <- remotePeers.toAkkaSource.evalMap(remotePeersSource =>
        BlockchainNetwork
          .make[F](
            localPeer.localAddress.getHostName,
            localPeer.localAddress.getPort,
            localPeer,
            remotePeersSource,
            clientHandler,
            peerServerF,
            peerFlowModifier
          )
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
                      .resource(DroppingTopic(blockAdoptionsTopic, 10))
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
      rpcInterpreter <- DroppingTopic(blockAdoptionsTopic, 10)
        .flatMap(_.subscribeAwaitUnbounded)
        .evalMap(
          ToplRpcServer.make(
            headerStore,
            bodyStore,
            transactionStore,
            mempool,
            transactionSyntaxValidation,
            localChain,
            blockHeights,
            blockIdTree,
            _
          )
        )
      _ <-
        ToplGrpc.Server
          .serve(rpcHost, rpcPort, rpcInterpreter)
          .evalTap(rpcServer =>
            Logger[F].info(s"RPC Server bound at ${rpcServer.getListenSockets.asScala.toList.mkString(",")}")
          )
      _ <- Async[F].background(
        mintedBlockStream
          .evalMap(block =>
            blockIdTree.associate(block.header.id, block.header.parentHeaderId) &>
            headerStore.put(block.header.id, block.header) &>
            bodyStore.put(block.header.id, block.body) &>
            ed25519VrfResource
              .use(implicit e => block.header.slotData.pure[F])
              .map(ReplaceModelUtil.slotDataFromLegacy)
              .flatTap(slotDataStore.put(block.header.id, _))
          )
          .map(Validated.Valid(_))
          .evalTap(localChain.adopt)
          .compile
          .drain
      )
      _ <- Resource.eval(p2pFiber.joinWithUnit)
    } yield ()
  }

}
