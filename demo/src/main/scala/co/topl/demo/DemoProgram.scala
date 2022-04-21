package co.topl.demo

import akka.actor.typed.ActorSystem
import akka.stream.scaladsl.{BroadcastHub, Flow, Keep, MergeHub, Sink, Source}
import akka.util.ByteString
import cats.data.{EitherT, OptionT, Validated}
import cats.effect._
import cats.implicits._
import cats.{Applicative, MonadThrow, Parallel, Show}
import co.topl.algebras.{Store, StoreReader, UnsafeResource}
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras.{BlockHeaderValidationAlgebra, LocalChainAlgebra}
import co.topl.consensus.{BlockHeaderV2Ops, BlockHeaderValidationFailure}
import co.topl.crypto.signing.Ed25519VRF
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.minting.algebras.PerpetualBlockMintAlgebra
import co.topl.models._
import co.topl.networking.blockchain._
import co.topl.networking.p2p.{ConnectedPeer, DisconnectedPeer, LocalPeer}
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

import scala.util.Random

object DemoProgram {

  /**
   * A forever-running program which traverses epochs and the slots within the epochs
   */
  def run[F[_]: Parallel: MonadThrow: Logger: Async: FToFuture](
    mint:               Option[PerpetualBlockMintAlgebra[F]],
    headerValidation:   BlockHeaderValidationAlgebra[F],
    headerStore:        Store[F, TypedIdentifier, BlockHeaderV2],
    bodyStore:          Store[F, TypedIdentifier, BlockBodyV2],
    transactionStore:   Store[F, TypedIdentifier, Transaction],
    slotDataStore:      StoreReader[F, TypedIdentifier, SlotData],
    localChain:         LocalChainAlgebra[F],
    blockIdTree:        ParentChildTree[F, TypedIdentifier],
    blockHeights:       EventSourcedState[F, TypedIdentifier, Long => F[Option[TypedIdentifier]]],
    ed25519VrfResource: UnsafeResource[F, Ed25519VRF],
    host:               String,
    bindPort:           Int,
    localPeer:          LocalPeer,
    remotePeers:        Source[DisconnectedPeer, _],
    peerFlowModifier: (
      ConnectedPeer,
      Flow[ByteString, ByteString, F[BlockchainPeerClient[F]]]
    ) => Flow[ByteString, ByteString, F[BlockchainPeerClient[F]]]
  )(implicit system: ActorSystem[_], random: Random): F[Unit] =
    for {
      (locallyAdoptedBlocksSource, locallyAdoptedBlocksSink) <-
        BroadcastHub.sink[TypedIdentifier].preMaterialize().pure[F]
      blockProcessor =
        (blockV2: BlockV2) =>
          MonadThrow[F].recoverWith(
            processBlock[F](
              blockV2,
              headerValidation,
              headerStore,
              bodyStore,
              localChain,
              blockIdTree,
              ed25519VrfResource
            )
          ) { case t =>
            Logger[F].error(t)(show"Failed to process block id=${blockV2.headerV2.id}").as(false)
          }
      (remoteBlockSink, remoteBlockSource) <-
        MergeHub
          .source[BlockV2]
          .preMaterialize()
          .pure[F]
      clientHandler <- BlockchainClientHandler.FetchAllBlocks.make[F](
        headerStore,
        bodyStore,
        transactionStore,
        blockIdTree,
        remoteBlockSink,
        blockHeights,
        localChain,
        slotDataStore
      )
      peerServer <- BlockchainPeerServer.FromStores.make(
        headerStore,
        bodyStore,
        transactionStore,
        blockHeights,
        localChain,
        locallyAdoptedBlocksSource
      )
      (p2pServer, p2pFiber) <- BlockchainNetwork
        .make[F](host, bindPort, localPeer, remotePeers, clientHandler, peerServer, peerFlowModifier)
      mintedBlockStream <- mint.fold(Source.never[BlockV2].pure[F])(_.blocks)
      streamCompletionFuture =
        mintedBlockStream
          .tapAsyncF(1)(block => Logger[F].info(show"Minted block ${block.headerV2}"))
          // Prioritize locally minted blocks over remote blocks
          .mergePreferred(remoteBlockSource, priority = false)
          .mapAsyncF(1)(block => blockProcessor(block).tupleLeft(block.headerV2.id.asTypedBytes))
          .collect { case (id, true) => id }
          .alsoTo(locallyAdoptedBlocksSink)
          .toMat(Sink.ignore)(Keep.right)
          .liftTo[F]
      _ <- Async[F].fromFuture(streamCompletionFuture)
      _ <- p2pFiber.join
    } yield ()

  implicit private val showBlockHeaderValidationFailure: Show[BlockHeaderValidationFailure] =
    Show.fromToString

  /**
   * Insert block to local storage and perform chain selection.  If better, validate the block and then adopt it locally.
   */
  private def processBlock[F[_]: MonadThrow: Sync: Logger](
    block:              BlockV2,
    headerValidation:   BlockHeaderValidationAlgebra[F],
    headerStore:        Store[F, TypedIdentifier, BlockHeaderV2],
    bodyStore:          Store[F, TypedIdentifier, BlockBodyV2],
    localChain:         LocalChainAlgebra[F],
    blockIdTree:        ParentChildTree[F, TypedIdentifier],
    ed25519VrfResource: UnsafeResource[F, Ed25519VRF]
  ): F[Boolean] =
    for {
      _ <- headerStore
        .contains(block.headerV2.id)
        .ifM(Applicative[F].unit, headerStore.put(block.headerV2.id, block.headerV2))
      _ <- bodyStore
        .contains(block.headerV2.id)
        .ifM(Applicative[F].unit, bodyStore.put(block.headerV2.id, block.blockBodyV2))
      _        <- blockIdTree.associate(block.headerV2.id, block.headerV2.parentHeaderId)
      slotData <- ed25519VrfResource.use(implicit ed25519Vrf => block.headerV2.slotData.pure[F])
      adopted <-
        localChain
          .isWorseThan(slotData)
          .ifM(
            Sync[F].defer(
              EitherT(
                OptionT(headerStore.get(block.headerV2.parentHeaderId))
                  .getOrElseF(MonadThrow[F].raiseError(new NoSuchElementException(block.headerV2.parentHeaderId.show)))
                  .flatMap(parent => headerValidation.validate(block.headerV2, parent))
              )
                .foldF(
                  e =>
                    Logger[F]
                      .warn(show"Invalid block header. reason=$e block=${block.headerV2}")
                      // TODO: Penalize the peer
                      .flatTap(_ =>
                        headerStore.remove(block.headerV2.id).tupleRight(bodyStore.remove(block.headerV2.id))
                      )
                      .as(false),
                  header =>
                    (localChain.adopt(Validated.Valid(slotData)) >>
                    Logger[F].info(
                      show"Adopted head block id=${header.id.asTypedBytes} height=${header.height} slot=${header.slot}"
                    )).as(true)
                )
            ),
            Sync[F]
              .defer(Logger[F].info(show"Ignoring weaker block header id=${block.headerV2.id.asTypedBytes}"))
              .as(false)
          )
    } yield adopted

}
