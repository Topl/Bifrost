package co.topl.simulator.eligibility

import cats.data.{EitherT, OptionT, Validated}
import cats.effect.{Clock, Sync}
import cats.implicits._
import cats.{Monad, MonadError, Parallel, Show}
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras.{ClockAlgebra, Stats, Store, UnsafeResource}
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras.{BlockHeaderValidationAlgebra, EtaCalculationAlgebra, LocalChainAlgebra}
import co.topl.consensus.{BlockHeaderV2Ops, BlockHeaderValidationFailure}
import co.topl.crypto.signing.Ed25519VRF
import co.topl.minting.algebras.BlockMintAlgebra
import co.topl.models._
import co.topl.typeclasses.implicits._
import io.circe.Json
import io.circe.syntax.EncoderOps
import org.typelevel.log4cats.Logger

object EligibilitySimulatorProgram {

  /**
   * A program which mints blocks up to a certain height.  This program is not beholden to a real-world clock; there is
   * no delay between the processing of slots.
   */
  def run[F[_]: MonadError[*[_], Throwable]: Logger: Parallel: Clock: Sync](
    clock:              ClockAlgebra[F],
    mints:              List[BlockMintAlgebra[F]],
    headerValidation:   BlockHeaderValidationAlgebra[F],
    headerStore:        Store[F, TypedIdentifier, BlockHeaderV2],
    blockStore:         Store[F, TypedIdentifier, BlockV2],
    etaCalculation:     EtaCalculationAlgebra[F],
    localChain:         LocalChainAlgebra[F],
    ed25519VrfResource: UnsafeResource[F, Ed25519VRF],
    stats:              Stats[F],
    statsName:          String,
    toHeight:           Long
  ): F[Unit] =
    for {
      _ <- (0L, 0L).iterateWhileM { case (epoch, h) =>
        handleEpoch(
          epoch,
          clock,
          mints,
          headerValidation,
          headerStore,
          blockStore,
          etaCalculation,
          localChain,
          ed25519VrfResource,
          stats,
          statsName,
          toHeight
        )
          .as(epoch + 1)
          .flatMap(e => localChain.head.map(e -> _.height))
      } { case (_, currentHeight) => currentHeight < toHeight }.void
    } yield ()

  /**
   * Perform the operations for an epoch
   */
  private def handleEpoch[F[_]: MonadError[*[_], Throwable]: Logger: Parallel: Sync](
    epoch:              Epoch,
    clock:              ClockAlgebra[F],
    mints:              List[BlockMintAlgebra[F]],
    headerValidation:   BlockHeaderValidationAlgebra[F],
    headerStore:        Store[F, TypedIdentifier, BlockHeaderV2],
    blockStore:         Store[F, TypedIdentifier, BlockV2],
    etaCalculation:     EtaCalculationAlgebra[F],
    localChain:         LocalChainAlgebra[F],
    ed25519VrfResource: UnsafeResource[F, Ed25519VRF],
    stats:              Stats[F],
    statsName:          String,
    toHeight:           Long
  ): F[Unit] =
    for {
      boundary <- clock.epochRange(epoch)
      _        <- startEpoch(epoch, boundary, headerStore, localChain, etaCalculation)
      _ <- traverseEpoch(
        boundary,
        clock,
        mints,
        headerValidation,
        headerStore,
        blockStore,
        localChain,
        ed25519VrfResource,
        stats,
        statsName,
        toHeight
      )
      _ <- finishEpoch[F](epoch)
    } yield ()

  /**
   * Perform operations at the start of the epoch
   */
  private def startEpoch[F[_]: MonadError[*[_], Throwable]: Logger: Parallel](
    epoch:          Epoch,
    boundary:       ClockAlgebra.EpochBoundary,
    headerStore:    Store[F, TypedIdentifier, BlockHeaderV2],
    localChain:     LocalChainAlgebra[F],
    etaCalculation: EtaCalculationAlgebra[F]
  ): F[Unit] =
    for {
      _      <- Logger[F].info(show"Starting epoch=$epoch (${boundary.start}..${boundary.end})")
      headId <- localChain.head.map(_.slotId.blockId)
      head <- OptionT(headerStore.get(headId))
        .getOrElseF(new IllegalStateException(show"Missing blockId=$headId").raiseError[F, BlockHeaderV2])
      _ <- etaCalculation.etaToBe(head.slotId, boundary.start)
    } yield ()

  /**
   * Iterate through the epoch slot-by-slot
   */
  private def traverseEpoch[F[_]: MonadError[*[_], Throwable]: Logger: Parallel: Sync](
    boundary:           ClockAlgebra.EpochBoundary,
    clock:              ClockAlgebra[F],
    mints:              List[BlockMintAlgebra[F]],
    headerValidation:   BlockHeaderValidationAlgebra[F],
    headerStore:        Store[F, TypedIdentifier, BlockHeaderV2],
    blockStore:         Store[F, TypedIdentifier, BlockV2],
    localChain:         LocalChainAlgebra[F],
    ed25519VrfResource: UnsafeResource[F, Ed25519VRF],
    stats:              Stats[F],
    statsName:          String,
    toHeight:           Long
  ): F[Unit] =
    localChain.head
      .map(_.height)
      .flatMap(currentHeight =>
        (boundary.start: Long, currentHeight).iterateUntilM { case (localSlot, _) =>
          for {
            _ <- processSlot(
              localSlot,
              mints,
              headerValidation,
              headerStore,
              blockStore,
              localChain,
              ed25519VrfResource,
              stats,
              statsName
            )
            h <- localChain.head.map(_.height)
          } yield (localSlot + 1, h)
        } { case (s, h) => s >= (boundary.end: Slot) || h >= toHeight }.void
      )

  /**
   * For each minter, attempt to mint.  For each minted block, process it.
   */
  private def processSlot[F[_]: MonadError[*[_], Throwable]: Logger: Parallel: Sync](
    slot:               Slot,
    mints:              List[BlockMintAlgebra[F]],
    headerValidation:   BlockHeaderValidationAlgebra[F],
    headerStore:        Store[F, TypedIdentifier, BlockHeaderV2],
    blockStore:         Store[F, TypedIdentifier, BlockV2],
    localChain:         LocalChainAlgebra[F],
    ed25519VrfResource: UnsafeResource[F, Ed25519VRF],
    stats:              Stats[F],
    statsName:          String
  ): F[Unit] =
    for {
      _ <- Logger[F].debug(show"Processing slot=$slot")
      canonicalHead <- localChain.head
        .flatMap(slotData =>
          OptionT(headerStore.get(slotData.slotId.blockId))
            .getOrElseF(new IllegalStateException("BlockNotFound").raiseError[F, BlockHeaderV2])
        )
      mintedBlocks <- mints.parTraverse(_.attemptMint(canonicalHead, Nil, slot)).map(_.flatten)
      _ <- mintedBlocks.traverse(
        processMintedBlock(
          _,
          headerValidation,
          blockStore,
          localChain,
          ed25519VrfResource,
          stats,
          statsName,
          canonicalHead
        )
      )
    } yield ()

  implicit private val showBlockHeaderValidationFailure: Show[BlockHeaderValidationFailure] =
    Show.fromToString

  /**
   * Insert block to local storage and perform chain selection.  If better, validate the block and then adopt it locally.
   */
  private def processMintedBlock[F[_]: Monad: Logger](
    nextBlock:          BlockV2,
    headerValidation:   BlockHeaderValidationAlgebra[F],
    blockStore:         Store[F, TypedIdentifier, BlockV2],
    localChain:         LocalChainAlgebra[F],
    ed25519VrfResource: UnsafeResource[F, Ed25519VRF],
    stats:              Stats[F],
    statsName:          String,
    canonicalHead:      BlockHeaderV2
  ): F[Unit] =
    for {
      _                     <- Logger[F].info(show"Minted block ${nextBlock.headerV2}")
      _                     <- blockStore.put(nextBlock.headerV2.id, nextBlock)
      slotData              <- ed25519VrfResource.use(implicit ed25519Vrf => nextBlock.headerV2.slotData.pure[F])
      localChainIsWorseThan <- localChain.isWorseThan(slotData)
      _ <-
        if (localChainIsWorseThan)
          EitherT(headerValidation.validate(nextBlock.headerV2, canonicalHead))
            // TODO: Now fetch the body from the network and validate against the ledger
            .semiflatTap(_ => localChain.adopt(Validated.Valid(slotData)))
            .semiflatTap(_ =>
              stats.write(
                statsName,
                Json.obj(
                  "h" -> nextBlock.headerV2.height.asJson,
                  "s" -> nextBlock.headerV2.slot.asJson
                )
              )
            )
            .semiflatTap(header => Logger[F].info(show"Adopted local head block id=${header.id}"))
            .void
            .valueOrF(e =>
              Logger[F]
                .warn(show"Invalid block header. reason=$e block=${nextBlock.headerV2}")
                // TODO: Penalize the peer
                .flatTap(_ => blockStore.remove(nextBlock.headerV2.id))
            )
        else
          Logger[F].info(show"Ignoring weaker block header id=${nextBlock.headerV2.id}")
    } yield ()

  /**
   * Perform operations at the completion of an epoch
   */
  private def finishEpoch[F[_]: Monad: Logger](epoch: Epoch): F[Unit] =
    for {
      _ <- Logger[F].info(show"Finishing epoch=$epoch")
    } yield ()

}
