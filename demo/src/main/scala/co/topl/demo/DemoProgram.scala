package co.topl.demo

import cats.data._
import cats.effect._
import cats.implicits._
import cats.{Monad, MonadError, Parallel}
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras.{ClockAlgebra, ConsensusState}
import co.topl.consensus.algebras.{BlockHeaderValidationAlgebra, EtaCalculationAlgebra}
import co.topl.consensus.{LocalChain, SlotData}
import co.topl.crypto.signatures.Ed25519VRF
import co.topl.minting.algebras.{BlockMintAlgebra, VrfProofAlgebra}
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

object DemoProgram {

  /**
   * A forever-running program which traverses epochs and the slots within the epochs
   */
  def run[F[_]: MonadError[*[_], Throwable]: Logger: Parallel: Clock: Sync](
    clock:            ClockAlgebra[F],
    mints:            List[BlockMintAlgebra[F]],
    headerValidation: BlockHeaderValidationAlgebra[F],
    vrfProofs:        List[VrfProofAlgebra[F]],
    state:            ConsensusState[F],
    etaCalculation:   EtaCalculationAlgebra[F],
    localChain:       LocalChain[F]
  ): F[Unit] =
    for {
      ed25519VrfRef <- Ref.of[F, Ed25519VRF](Ed25519VRF.precomputed())
      initialSlot   <- clock.globalSlot().map(_.min(1L))
      initialEpoch  <- clock.epochOf(initialSlot)
      _ <- initialEpoch
        .iterateForeverM(epoch =>
          handleEpoch(
            epoch,
            Option.when(epoch === initialEpoch)(initialSlot),
            clock,
            mints,
            headerValidation,
            vrfProofs,
            state,
            etaCalculation,
            localChain,
            ed25519VrfRef
          )
            .as(epoch + 1)
        )
        .void
    } yield ()

  /**
   * Perform the operations for an epoch
   */
  private def handleEpoch[F[_]: MonadError[*[_], Throwable]: Logger: Parallel: Sync](
    epoch:            Epoch,
    fromSlot:         Option[Slot],
    clock:            ClockAlgebra[F],
    mints:            List[BlockMintAlgebra[F]],
    headerValidation: BlockHeaderValidationAlgebra[F],
    vrfProofs:        List[VrfProofAlgebra[F]],
    state:            ConsensusState[F],
    etaCalculation:   EtaCalculationAlgebra[F],
    localChain:       LocalChain[F],
    ed25519VrfRef:    Ref[F, Ed25519VRF]
  ): F[Unit] =
    for {
      boundary <- clock.epochRange(epoch)
      _        <- startEpoch(epoch, boundary, state, vrfProofs, localChain, etaCalculation)
      _ <- traverseEpoch(
        fromSlot,
        boundary,
        clock,
        mints,
        headerValidation,
        state,
        localChain,
        ed25519VrfRef
      )
      _ <- finishEpoch(epoch, state)
    } yield ()

  /**
   * Perform operations at the start of the epoch
   */
  private def startEpoch[F[_]: MonadError[*[_], Throwable]: Logger: Parallel](
    epoch:          Epoch,
    boundary:       ClockAlgebra.EpochBoundary,
    state:          ConsensusState[F],
    vrfProofs:      List[VrfProofAlgebra[F]],
    localChain:     LocalChain[F],
    etaCalculation: EtaCalculationAlgebra[F]
  ): F[Unit] =
    for {
      _      <- Logger[F].info(s"Starting epoch=$epoch (${boundary.start}..${boundary.end})")
      _      <- Logger[F].info("Precomputing VRF data")
      headId <- localChain.head.map(_.slotId.blockId)
      head <- OptionT(state.lookupBlockHeader(headId))
        .getOrElseF(new IllegalStateException(show"Missing blockId=$headId").raiseError[F, BlockHeaderV2])
      nextEta <- etaCalculation.etaToBe(head.slotId, boundary.start)
      _       <- vrfProofs.traverse(_.precomputeForEpoch(epoch, nextEta))
    } yield ()

  /**
   * Iterate through the epoch slot-by-slot
   */
  private def traverseEpoch[F[_]: MonadError[*[_], Throwable]: Logger: Parallel: Sync](
    fromSlot:         Option[Slot],
    boundary:         ClockAlgebra.EpochBoundary,
    clock:            ClockAlgebra[F],
    mints:            List[BlockMintAlgebra[F]],
    headerValidation: BlockHeaderValidationAlgebra[F],
    state:            ConsensusState[F],
    localChain:       LocalChain[F],
    ed25519VrfRef:    Ref[F, Ed25519VRF]
  ): F[Unit] =
    fromSlot
      .getOrElse(boundary.start)
      .iterateUntilM(localSlot =>
        for {
          _ <- clock.delayedUntilSlot(localSlot)
          _ <- processSlot(localSlot, mints, headerValidation, state, localChain, ed25519VrfRef)
        } yield localSlot + 1
      )(_ >= boundary.end)
      .void

  private def processSlot[F[_]: MonadError[*[_], Throwable]: Logger: Parallel: Sync](
    slot:             Slot,
    mints:            List[BlockMintAlgebra[F]],
    headerValidation: BlockHeaderValidationAlgebra[F],
    state:            ConsensusState[F],
    localChain:       LocalChain[F],
    ed25519VrfRef:    Ref[F, Ed25519VRF]
  ): F[Unit] =
    for {
      _ <- Logger[F].debug(s"Processing slot=$slot")
      canonicalHead <- localChain.head
        .flatMap(slotData =>
          OptionT(state.lookupBlock(slotData.slotId.blockId))
            .getOrElseF(new IllegalStateException("BlockNotFound").raiseError[F, BlockV2])
        )
      mintedBlocks <- mints.traverse(_.mint(canonicalHead.headerV2, Nil, slot)).map(_.flatten)
      _ <- mintedBlocks.traverse(
        processMintedBlock(_, headerValidation, state, localChain, ed25519VrfRef, canonicalHead)
      )
    } yield ()

  /**
   * Insert block to local storage and perform chain selection.  If better, validate the block and then adopt it locally.
   */
  private def processMintedBlock[F[_]: Monad: Logger](
    nextBlock:        BlockV2,
    headerValidation: BlockHeaderValidationAlgebra[F],
    state:            ConsensusState[F],
    localChain:       LocalChain[F],
    ed25519VrfRef:    Ref[F, Ed25519VRF],
    canonicalHead:    BlockV2
  ): F[Unit] =
    for {
      _                     <- Logger[F].info(s"Minted block ${nextBlock.headerV2.show}")
      _                     <- state.append(nextBlock)
      slotData              <- ed25519VrfRef.modify(implicit ed25519Vrf => ed25519Vrf -> SlotData(nextBlock.headerV2))
      localChainIsWorseThan <- localChain.isWorseThan(slotData)
      _ <-
        if (localChainIsWorseThan)
          EitherT(headerValidation.validate(nextBlock.headerV2, canonicalHead.headerV2))
            .semiflatTap(_ => localChain.adopt(slotData))
            .semiflatTap(header => Logger[F].info(show"Adopted local head block id=${header.id}"))
            .void
            .valueOrF(e => Logger[F].warn(s"Invalid block header. reason=$e block=${nextBlock.headerV2.show}"))
        else
          Logger[F].info(show"Ignoring weaker block header id=${nextBlock.headerV2.id}")
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
