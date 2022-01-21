package co.topl.demo

import cats.data.{EitherT, OptionT, Validated}
import cats.effect._
import cats.implicits._
import cats.{Monad, MonadError, Parallel, Show}
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras.{ClockAlgebra, ConsensusState, Store, UnsafeResource}
import co.topl.consensus.algebras.{BlockHeaderValidationAlgebra, EtaCalculationAlgebra, LocalChainAlgebra}
import co.topl.consensus.{BlockHeaderValidationFailure, SlotData}
import co.topl.crypto.signing.Ed25519VRF
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
    clock:              ClockAlgebra[F],
    mints:              List[BlockMintAlgebra[F]],
    headerValidation:   BlockHeaderValidationAlgebra[F],
    state:              ConsensusState[F],
    headerStore:        Store[F, BlockHeaderV2],
    blockStore:         Store[F, BlockV2],
    etaCalculation:     EtaCalculationAlgebra[F],
    localChain:         LocalChainAlgebra[F],
    ed25519VrfResource: UnsafeResource[F, Ed25519VRF]
  ): F[Unit] =
    for {
      initialSlot  <- clock.globalSlot.map(_.min(1L))
      initialEpoch <- clock.epochOf(initialSlot)
      _ <- initialEpoch
        .iterateForeverM(epoch =>
          handleEpoch(
            epoch,
            Option.when(epoch === initialEpoch)(initialSlot),
            clock,
            mints,
            headerValidation,
            state,
            headerStore,
            blockStore,
            etaCalculation,
            localChain,
            ed25519VrfResource
          )
            .as(epoch + 1)
        )
        .void
    } yield ()

  /**
   * Perform the operations for an epoch
   */
  private def handleEpoch[F[_]: MonadError[*[_], Throwable]: Logger: Parallel: Sync](
    epoch:              Epoch,
    fromSlot:           Option[Slot],
    clock:              ClockAlgebra[F],
    mints:              List[BlockMintAlgebra[F]],
    headerValidation:   BlockHeaderValidationAlgebra[F],
    state:              ConsensusState[F],
    headerStore:        Store[F, BlockHeaderV2],
    blockStore:         Store[F, BlockV2],
    etaCalculation:     EtaCalculationAlgebra[F],
    localChain:         LocalChainAlgebra[F],
    ed25519VrfResource: UnsafeResource[F, Ed25519VRF]
  ): F[Unit] =
    for {
      boundary <- clock.epochRange(epoch)
      _        <- startEpoch(epoch, boundary, headerStore, localChain, etaCalculation)
      _ <- traverseEpoch(
        fromSlot,
        boundary,
        clock,
        mints,
        headerValidation,
        headerStore,
        blockStore,
        localChain,
        ed25519VrfResource
      )
      _ <- finishEpoch(epoch, state)
    } yield ()

  /**
   * Perform operations at the start of the epoch
   */
  private def startEpoch[F[_]: MonadError[*[_], Throwable]: Logger: Parallel](
    epoch:          Epoch,
    boundary:       ClockAlgebra.EpochBoundary,
    headerStore:    Store[F, BlockHeaderV2],
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
    fromSlot:           Option[Slot],
    boundary:           ClockAlgebra.EpochBoundary,
    clock:              ClockAlgebra[F],
    mints:              List[BlockMintAlgebra[F]],
    headerValidation:   BlockHeaderValidationAlgebra[F],
    headerStore:        Store[F, BlockHeaderV2],
    blockStore:         Store[F, BlockV2],
    localChain:         LocalChainAlgebra[F],
    ed25519VrfResource: UnsafeResource[F, Ed25519VRF]
  ): F[Unit] =
    fromSlot
      .getOrElse(boundary.start)
      .iterateUntilM(localSlot =>
        for {
          _ <- clock.delayedUntilSlot(localSlot)
          _ <- processSlot(localSlot, mints, headerValidation, headerStore, blockStore, localChain, ed25519VrfResource)
        } yield localSlot + 1
      )(_ >= boundary.end)
      .void

  /**
   * For each minter, attempt to mint.  For each minted block, process it.
   */
  private def processSlot[F[_]: MonadError[*[_], Throwable]: Logger: Parallel: Sync](
    slot:               Slot,
    mints:              List[BlockMintAlgebra[F]],
    headerValidation:   BlockHeaderValidationAlgebra[F],
    headerStore:        Store[F, BlockHeaderV2],
    blockStore:         Store[F, BlockV2],
    localChain:         LocalChainAlgebra[F],
    ed25519VrfResource: UnsafeResource[F, Ed25519VRF]
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
        processMintedBlock(_, headerValidation, blockStore, localChain, ed25519VrfResource, canonicalHead)
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
    blockStore:         Store[F, BlockV2],
    localChain:         LocalChainAlgebra[F],
    ed25519VrfResource: UnsafeResource[F, Ed25519VRF],
    canonicalHead:      BlockHeaderV2
  ): F[Unit] =
    for {
      _                     <- Logger[F].info(show"Minted block ${nextBlock.headerV2}")
      _                     <- blockStore.put(nextBlock)
      slotData              <- ed25519VrfResource.use(implicit ed25519Vrf => SlotData(nextBlock.headerV2))
      localChainIsWorseThan <- localChain.isWorseThan(slotData)
      _ <-
        if (localChainIsWorseThan)
          EitherT(headerValidation.validate(nextBlock.headerV2, canonicalHead))
            // TODO: Now fetch the body from the network and validate against the ledger
            .semiflatTap(_ => localChain.adopt(Validated.Valid(slotData)))
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
  private def finishEpoch[F[_]: Monad: Logger](epoch: Epoch, state: ConsensusState[F]): F[Unit] =
    for {
      _ <- Logger[F].info(show"Finishing epoch=$epoch")
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
