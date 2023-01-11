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
import co.topl.consensus.BlockHeaderOps
import co.topl.consensus.algebras.{BlockHeaderValidationAlgebra, LocalChainAlgebra}
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.grpc.ToplGrpc
import co.topl.ledger.algebras._
import co.topl.minting.algebras.StakingAlgebra
import co.topl.models._
import co.topl.networking.blockchain._
import co.topl.networking.p2p.{ConnectedPeer, DisconnectedPeer, LocalPeer}
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}
import BlockchainPeerHandler.monoidBlockchainPeerHandler
import co.topl.blockchain.algebras.BlockHeaderToBodyValidationAlgebra
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
  def run[F[_]: Parallel: Async: FToFuture: RunnableGraphToF](
    clock:                       ClockAlgebra[F],
    staker:                      Option[StakingAlgebra[F]],
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
  )(implicit system: ActorSystem[_], random: Random): F[Unit] = {
    implicit val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromClass[F](Blockchain.getClass)
    for {
      (localChain, blockAdoptionsTopic)    <- LocalChainBroadcaster.make(_localChain)
      (mempool, transactionAdoptionsTopic) <- MempoolBroadcaster.make(_mempool)
      _ <- (blockAdoptionsTopic.toAkkaBroadcastSource, transactionAdoptionsTopic.toAkkaBroadcastSource).tupled.flatMap {
        case (blockAdoptionsSource, transactionAdoptionsSource) =>
          for {
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
            peerServer <- Resource.eval(
              BlockchainPeerServer.FromStores.make(
                slotDataStore,
                headerStore,
                bodyStore,
                transactionStore,
                blockHeights,
                localChain,
                blockAdoptionsSource
                  .tapAsyncF(1)(id => Logger[F].debug(show"Broadcasting block id=$id to peer"))
                  .pure[F],
                transactionAdoptionsSource
                  .tapAsyncF(1)(id => Logger[F].debug(show"Broadcasting transaction id=$id to peer"))
                  .pure[F]
              )
            )
            (p2pServer, p2pFiber) <- remotePeers.toAkkaSource.evalMap(remotePeersSource =>
              BlockchainNetwork
                .make[F](
                  localPeer.localAddress.getHostName,
                  localPeer.localAddress.getPort,
                  localPeer,
                  remotePeersSource,
                  clientHandler,
                  peerServer,
                  peerFlowModifier
                )
            )
            blockPacker <- Resource.eval(
              BlockPacker.make[F](
                mempool,
                transactionStore.getOrRaise,
                BlockPacker.makeBodyValidator(bodySyntaxValidation, bodySemanticValidation, bodyAuthorizationValidation)
              )
            )
            mintedBlockStream =
              staker.fold[Stream[F, Block]](Stream.never[F])(staker =>
                // The BlockProducer needs a stream/Source of "parents" upon which it should build.  This stream is the
                // concatenation of the current local head with the stream of local block adoptions
                Stream
                  .eval(localChain.head)
                  .flatMap(currentHead =>
                    Stream.resource(
                      (
                        Stream.eval(clock.delayedUntilSlot(currentHead.slotId.slot).as(currentHead))
                        ++ blockAdoptionsTopic.subscribeDropOldest(1).evalMap(slotDataStore.getOrRaise)
                      ).toAkkaSource
                    )
                  )
                  .flatMap(adoptionsSource =>
                    Stream
                      .eval(
                        BlockProducer
                          .make[F](
                            adoptionsSource,
                            staker,
                            clock,
                            blockPacker
                          )
                          .flatMap(_.blocks)
                      )
                      .flatMap(_.asFS2Stream)
                  )
              )
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
                blockAdoptionsTopic.subscribeDropOldest(10)
              )
            )
            rpcServer <- ToplGrpc.Server.serve(rpcHost, rpcPort, rpcInterpreter)
            mintedBlockStreamCompletionF =
              mintedBlockStream
                .evalTap(block => Logger[F].info(show"Minted header=${block.header} body=${block.body}"))
                .evalMap(block =>
                  blockIdTree.associate(block.header.id, block.header.parentHeaderId) >>
                  headerStore.put(block.header.id, block.header) >>
                  bodyStore.put(block.header.id, block.body) >>
                  ed25519VrfResource
                    .use(implicit e => block.header.slotData.pure[F])
                    .flatTap(slotDataStore.put(block.header.id, _))
                )
                .evalTap(slotData =>
                  Logger[F].info(
                    show"Adopted head block id=${slotData.slotId.blockId} height=${slotData.height} slot=${slotData.slotId.slot}"
                  )
                )
                .map(Validated.Valid(_))
                .evalTap(localChain.adopt)
                .compile
                .drain
            _ <-
              Resource.eval(
                Logger[F].info(s"RPC Server bound at ${rpcServer.getListenSockets.asScala.toList.mkString(",")}")
              )
            _ <- Resource.eval(mintedBlockStreamCompletionF)
            _ <- Resource.eval(p2pFiber.joinWithUnit)
          } yield ()
      }.use_
    } yield ()
  }

}
