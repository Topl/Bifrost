package co.topl.demo

import cats.data.{EitherT, OptionT}
import cats.implicits._
import cats.{Applicative, Monad, MonadError}
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras.{ClockAlgebra, ConsensusState}
import co.topl.consensus.algebras.{BlockHeaderValidationAlgebra, EtaCalculationAlgebra}
import co.topl.minting.algebras.{BlockMintAlgebra, VrfProofAlgebra}
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

object DemoProgram {

  /**
   * A forever-running program which traverses epochs and the slots within the epochs
   */
  def run[F[_]: MonadError[*[_], Throwable]: Logger](
    clock:            ClockAlgebra[F],
    mint:             BlockMintAlgebra[F],
    headerValidation: BlockHeaderValidationAlgebra[F],
    vrfProof:         VrfProofAlgebra[F],
    state:            ConsensusState[F],
    etaCalculation:   EtaCalculationAlgebra[F]
  ): F[Unit] =
    for {
      initialSlot  <- clock.currentSlot().map(_.max(1L))
      initialEpoch <- clock.epochOf(initialSlot)
      _ <- initialEpoch
        .iterateForeverM(epoch =>
          handleEpoch(
            epoch,
            Option.when(epoch === initialEpoch)(initialSlot),
            clock,
            mint,
            headerValidation,
            vrfProof,
            state,
            etaCalculation
          )
            .as(epoch + 1)
        )
        .void
    } yield ()

  /**
   * Perform the operations for an epoch
   */
  private def handleEpoch[F[_]: MonadError[*[_], Throwable]: Logger](
    epoch:            Epoch,
    fromSlot:         Option[Slot],
    clock:            ClockAlgebra[F],
    mint:             BlockMintAlgebra[F],
    headerValidation: BlockHeaderValidationAlgebra[F],
    vrfProof:         VrfProofAlgebra[F],
    state:            ConsensusState[F],
    etaCalculation:   EtaCalculationAlgebra[F]
  ): F[Unit] =
    for {
      boundary <- clock.epochRange(epoch)
      _        <- startEpoch(epoch, boundary, state, vrfProof)
      _        <- traverseEpoch(fromSlot, epoch, boundary, clock, mint, headerValidation, state, etaCalculation)
      _        <- finishEpoch(epoch, state)
    } yield ()

  /**
   * Perform operations at the start of the epoch
   */
  private def startEpoch[F[_]: MonadError[*[_], Throwable]: Logger](
    epoch:    Epoch,
    boundary: ClockAlgebra.EpochBoundary,
    state:    ConsensusState[F],
    vrfProof: VrfProofAlgebra[F]
  ): F[Unit] =
    for {
      _ <- Logger[F].info(s"Starting epoch=$epoch (${boundary.start}..${boundary.end})")
      _ <- Logger[F].info("Precomputing VRF data")
      previousEta <- OptionT(state.lookupEta(epoch - 1))
        .getOrElseF(new IllegalStateException(s"Unknown Eta for epoch=${epoch - 1}").raiseError[F, Eta])
      _ <- vrfProof.precomputeForEpoch(epoch, previousEta)
    } yield ()

  /**
   * Iterate through the epoch slot-by-slot
   */
  private def traverseEpoch[F[_]: MonadError[*[_], Throwable]: Logger](
    fromSlot:         Option[Slot],
    epoch:            Epoch,
    boundary:         ClockAlgebra.EpochBoundary,
    clock:            ClockAlgebra[F],
    mint:             BlockMintAlgebra[F],
    headerValidation: BlockHeaderValidationAlgebra[F],
    state:            ConsensusState[F],
    etaCalculation:   EtaCalculationAlgebra[F]
  ): F[Unit] = {
    val twoThirdsSlot = (boundary.length * 2 / 3) + boundary.start
    fromSlot
      .getOrElse(boundary.start)
      .iterateUntilM(slot =>
        for {
          _ <- clock.delayedUntilSlot(slot)
          _ <- processSlot(slot, mint, headerValidation, state)
          _ <- Applicative[F].whenA(slot === twoThirdsSlot)(handleTwoThirdsEvent(epoch, state, etaCalculation))
        } yield slot + 1
      )(_ >= boundary.end)
      .void
  }

  private def processSlot[F[_]: Monad: Logger](
    slot:             Slot,
    mint:             BlockMintAlgebra[F],
    headerValidation: BlockHeaderValidationAlgebra[F],
    state:            ConsensusState[F]
  ): F[Unit] =
    Logger[F].debug(s"Processing slot=$slot") >>
    state.canonicalHead
      .flatMap(canonicalHead =>
        // Attempt to mint a new block
        OptionT(mint.mint(canonicalHead.headerV2, Nil, slot))
          .semiflatMap(nextBlock =>
            // If a block was minted at this slot, attempt to validate it
            EitherT(headerValidation.validate(nextBlock.headerV2, canonicalHead.headerV2))
              .semiflatTap(_ => state.append(nextBlock))
              .semiflatTap(header => Logger[F].info(s"Appended block ${header.show}"))
              .void
              .valueOrF(e => Logger[F].warn(s"Invalid block header. reason=$e block=${nextBlock.headerV2.show}"))
          )
          .value
          .void
      )

  private def handleTwoThirdsEvent[F[_]: MonadError[*[_], Throwable]: Logger](
    epoch:          Epoch,
    state:          ConsensusState[F],
    etaCalculation: EtaCalculationAlgebra[F]
  ): F[Unit] =
    for {
      _       <- Logger[F].info(s"Handling 2/3 event for epoch=$epoch")
      nextEta <- etaCalculation.calculate(epoch)
      _       <- Logger[F].info(s"Computed eta=$nextEta for epoch=$epoch")
      _       <- state.writeEta(epoch, nextEta)
    } yield ()

  /**
   * Perform operations at the completion of an epoch
   */
  private def finishEpoch[F[_]: Monad: Logger](epoch: Epoch, state: ConsensusState[F]): F[Unit] =
    for {
      _ <- Logger[F].info(s"Finishing epoch=$epoch")
      _ <- Logger[F].info("Populating registrations for next epoch")
      newRegistrations <- state
        .foldRegistrations(epoch)(Map.empty[TaktikosAddress, Box.Values.TaktikosRegistration]) {
          case (acc, (address, registration)) => acc.updated(address, registration).pure[F]
        }
      _ <- state.writeRegistrations(epoch + 1, newRegistrations)
      _ <- Logger[F].info("Populating relative stake distributions for next epoch")
      newRelativeStakes <- state
        .foldRelativeStakes(epoch)(Map.empty[TaktikosAddress, Ratio]) { case (acc, (address, stake)) =>
          acc.updated(address, stake).pure[F]
        }
      _ <- state.writeRelativeStakes(epoch + 1, newRelativeStakes)
    } yield ()

}
