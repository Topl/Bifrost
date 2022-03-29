package co.topl.demo

import akka.actor.typed.ActorSystem
import akka.stream.scaladsl.{BroadcastHub, Keep, Sink, Source}
import cats.data.{EitherT, OptionT, Validated}
import cats.effect._
import cats.implicits._
import cats.{Applicative, MonadThrow, Parallel, Show}
import co.topl.algebras.{Store, UnsafeResource}
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras.{BlockHeaderValidationAlgebra, LocalChainAlgebra}
import co.topl.consensus.{BlockHeaderV2Ops, BlockHeaderValidationFailure}
import co.topl.crypto.signing.Ed25519VRF
import co.topl.minting.algebras.PerpetualBlockMintAlgebra
import co.topl.models._
import co.topl.networking.blockchain._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

import java.net.InetSocketAddress
import scala.util.Random

object DemoProgram {

  /**
   * A forever-running program which traverses epochs and the slots within the epochs
   */
  def run[F[_]: Parallel: MonadThrow: Logger: Async: FToFuture](
    mint:               Option[PerpetualBlockMintAlgebra[F]],
    headerValidation:   BlockHeaderValidationAlgebra[F],
    headerStore:        Store[F, BlockHeaderV2],
    bodyStore:          Store[F, BlockBodyV2],
    transactionStore:   Store[F, Transaction],
    localChain:         LocalChainAlgebra[F],
    ed25519VrfResource: UnsafeResource[F, Ed25519VRF],
    bindPort:           Int,
    remotePeers:        Source[InetSocketAddress, _]
  )(implicit system:    ActorSystem[_], random: Random): F[Unit] =
    for {
      ((offerLocallyAdoptedBlocks, locallyAdoptedBlocksCompletion), locallyAdoptedBlocksSource) <-
        Source
          .dropHeadQueue[F, TypedIdentifier](36)
          .toMat(BroadcastHub.sink[TypedIdentifier])(Keep.both)
          .liftTo[F]
      blockProcessor =
        (blockV2: BlockV2) =>
          processBlock[F](
            blockV2,
            headerValidation,
            headerStore,
            bodyStore,
            localChain,
            ed25519VrfResource
          )
      clientHandler <- BlockchainClientHandler.FetchAllBlocks.make[F](
        headerStore,
        bodyStore,
        transactionStore,
        block =>
          blockProcessor(block).flatMap(
            Applicative[F].whenA(_)(offerLocallyAdoptedBlocks(block.headerV2.id))
          )
      )
      peerServer <- BlockchainPeerServer.FromStores.make(
        headerStore,
        bodyStore,
        transactionStore,
        locallyAdoptedBlocksSource
      )
      (p2pServer, p2pFiber) <- BlockchainNetwork.make[F](bindPort, remotePeers, clientHandler, peerServer)
      mintedBlockStream     <- mint.fold(Source.never[BlockV2].pure[F])(_.blocks)
      streamCompletionFuture =
        mintedBlockStream
          .tapAsyncF(1)(block => Logger[F].info(show"Minted block ${block.headerV2}"))
          .mapAsyncF(1)(block => blockProcessor(block).tupleLeft(block.headerV2.id.asTypedBytes))
          .collect { case (id, true) => id }
          .tapAsyncF(1)(offerLocallyAdoptedBlocks)
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
    headerStore:        Store[F, BlockHeaderV2],
    bodyStore:          Store[F, BlockBodyV2],
    localChain:         LocalChainAlgebra[F],
    ed25519VrfResource: UnsafeResource[F, Ed25519VRF]
  ): F[Boolean] =
    for {
      _ <- headerStore
        .contains(block.headerV2.id)
        .ifM(Applicative[F].unit, headerStore.put(block.headerV2.id, block.headerV2))
      _ <- bodyStore
        .contains(block.headerV2.id)
        .ifM(Applicative[F].unit, bodyStore.put(block.headerV2.id, block.blockBodyV2))
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
                .semiflatTap(_ => localChain.adopt(Validated.Valid(slotData)))
                .semiflatTap(header =>
                  Logger[F].info(
                    show"Adopted head block id=${header.id.asTypedBytes} height=${header.height} slot=${header.slot}"
                  )
                )
                .as(true)
                .valueOrF(e =>
                  Logger[F]
                    .warn(show"Invalid block header. reason=$e block=${block.headerV2}")
                    // TODO: Penalize the peer
                    .flatTap(_ => headerStore.remove(block.headerV2.id).tupleRight(bodyStore.remove(block.headerV2.id)))
                    .as(false)
                )
            ),
            Sync[F]
              .defer(Logger[F].info(show"Ignoring weaker block header id=${block.headerV2.id.asTypedBytes}"))
              .as(false)
          )
    } yield adopted

}
