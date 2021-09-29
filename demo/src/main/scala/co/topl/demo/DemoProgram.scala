package co.topl.demo

import cats.data.{EitherT, OptionT}
import cats.implicits._
import cats.{Applicative, Monad, MonadError}
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras.{BlockchainState, ClockAlgebra, LoggerAlgebra}
import co.topl.consensus.algebras.{BlockHeaderValidationAlgebra, EtaCalculationAlgebra}
import co.topl.minting.algebras.{BlockMintAlgebra, VrfProofAlgebra}
import co.topl.models.utility.Ratio
import co.topl.models.{BlockHeaderV2, Box, Epoch, Slot, TaktikosAddress}
import co.topl.typeclasses.implicits._

object DemoProgram {

  def run[F[_]: MonadError[*[_], Throwable]: LoggerAlgebra](
    clock:            ClockAlgebra[F],
    mint:             BlockMintAlgebra[F],
    headerValidation: BlockHeaderValidationAlgebra[F],
    vrfProof:         VrfProofAlgebra[F],
    state:            BlockchainState[F],
    etaCalculation:   EtaCalculationAlgebra[F]
  ): F[Unit] =
    0
      .iterateForeverM(epoch =>
        handleEpoch(epoch, Option.when(epoch == 0)(1), clock, mint, headerValidation, vrfProof, state, etaCalculation)
          .as(epoch + 1)
      )
      .void

  private def handleEpoch[F[_]: MonadError[*[_], Throwable]: LoggerAlgebra](
    epoch:            Epoch,
    fromSlot:         Option[Slot],
    clock:            ClockAlgebra[F],
    mint:             BlockMintAlgebra[F],
    headerValidation: BlockHeaderValidationAlgebra[F],
    vrfProof:         VrfProofAlgebra[F],
    state:            BlockchainState[F],
    etaCalculation:   EtaCalculationAlgebra[F]
  ): F[Unit] =
    clock
      .epochRange(epoch)
      .flatMap(boundary =>
        startEpoch(epoch, boundary, state, vrfProof) >>
        traverseEpoch(fromSlot, epoch, boundary, clock, mint, headerValidation, state, etaCalculation) >>
        finishEpoch(epoch, state)
      )

  private def startEpoch[F[_]: MonadError[*[_], Throwable]: LoggerAlgebra](
    epoch:    Epoch,
    boundary: ClockAlgebra.EpochBoundary,
    state:    BlockchainState[F],
    vrfProof: VrfProofAlgebra[F]
  ): F[Unit] =
    LoggerAlgebra[F].info(s"Starting epoch=$epoch (${boundary.start}..${boundary.end})").pure[F] >>
    LoggerAlgebra[F].info("Precomputing VRF data").pure[F] >>
    state
      .lookupEta(epoch - 1)
      .flatMap(
        _.fold(new IllegalStateException(s"Unknown Eta for epoch=${epoch - 1}").raiseError[F, Unit])(
          vrfProof.precomputeForEpoch(epoch, _)
        )
      )

  private def finishEpoch[F[_]: Monad: LoggerAlgebra](epoch: Epoch, state: BlockchainState[F]): F[Unit] =
    LoggerAlgebra[F].info(s"Finishing epoch=$epoch").pure[F] >>
    LoggerAlgebra[F].info("Populating registrations for next epoch").pure[F] >>
    // TODO: This should pull data from a registry/state, but for now just copy from the previous epoch
    state
      .foldRegistrations(epoch)(Map.empty[TaktikosAddress, Box.Values.TaktikosRegistration]) {
        case (acc, (address, registration)) => acc.updated(address, registration).pure[F]
      }
      .flatMap(newRegistrations => state.writeRegistrations(epoch + 1, newRegistrations)) >>
    LoggerAlgebra[F].info("Populating relative stake distributions for next epoch").pure[F] >>
    // TODO: This should pull data from a registry/state, but for now just copy from the previous epoch
    state
      .foldRelativeStakes(epoch)(Map.empty[TaktikosAddress, Ratio]) { case (acc, (address, stake)) =>
        acc.updated(address, stake).pure[F]
      }
      .flatMap(newRelativeStakes => state.writeRelativeStakes(epoch + 1, newRelativeStakes))

  private def traverseEpoch[F[_]: MonadError[*[_], Throwable]: LoggerAlgebra](
    fromSlot:         Option[Slot],
    epoch:            Epoch,
    boundary:         ClockAlgebra.EpochBoundary,
    clock:            ClockAlgebra[F],
    mint:             BlockMintAlgebra[F],
    headerValidation: BlockHeaderValidationAlgebra[F],
    state:            BlockchainState[F],
    etaCalculation:   EtaCalculationAlgebra[F]
  ): F[Unit] =
    fromSlot
      .foldLeft(boundary.to(LazyList)) { case (boundary, fromSlot) => boundary.dropWhile(_ < fromSlot) }
      .traverse_(slot =>
        clock
          .delayedUntilSlot(slot) >>
        processSlot(slot, mint, headerValidation, state) >>
        Applicative[F].whenA((slot - boundary.start) === (boundary.length * 2 / 3))(
          handleTwoThirdsEvent(epoch, state, etaCalculation)
        )
      )

  private def processSlot[F[_]: MonadError[*[_], Throwable]: LoggerAlgebra](
    slot:             Slot,
    mint:             BlockMintAlgebra[F],
    headerValidation: BlockHeaderValidationAlgebra[F],
    state:            BlockchainState[F]
  ): F[Unit] =
    state.canonicalHead
      .flatMap(canonicalHead =>
        // Mint a new block
        OptionT(mint.mint(canonicalHead.headerV2, Nil, slot))
          .semiflatMap { nextBlock =>
            // If a block was minted at this slot, attempt to validate it
            EitherT(
              headerValidation
                .validate(nextBlock.headerV2, canonicalHead.headerV2)
            )
              .semiflatTap(_ => state.append(nextBlock))
              .semiflatTap(header => LoggerAlgebra[F].info(s"Appended block ${header.show}").pure[F])
              .valueOrF(e => new IllegalArgumentException(e.toString).raiseError[F, BlockHeaderV2])
          }
          .value
          .void
      )

  private def handleTwoThirdsEvent[F[_]: MonadError[*[_], Throwable]: LoggerAlgebra](
    epoch:          Epoch,
    state:          BlockchainState[F],
    etaCalculation: EtaCalculationAlgebra[F]
  ): F[Unit] =
    for {
      _       <- LoggerAlgebra[F].info(s"Handling 2/3 event for epoch=$epoch").pure[F]
      nextEta <- etaCalculation.calculate(epoch)
      _       <- state.writeEta(epoch, nextEta)
    } yield ()
}
