package co.topl.demo

import cats.data.OptionT
import cats.implicits._
import cats.{Monad, MonadError}
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras.{BlockchainState, ClockAlgebra}
import co.topl.consensus.algebras.BlockHeaderValidationAlgebra
import co.topl.minting.algebras.{BlockMintAlgebra, VrfProofAlgebra}
import co.topl.models.{Epoch, Slot}
import co.topl.typeclasses.implicits._

object DemoProgram {

  def run[F[_]: MonadError[*[_], Throwable]](
    epochs:           Int,
    clock:            ClockAlgebra[F],
    mint:             BlockMintAlgebra[F],
    headerValidation: BlockHeaderValidationAlgebra[F],
    vrfProof:         VrfProofAlgebra[F],
    state:            BlockchainState[F]
  ): F[Unit] = (0 to epochs)
    .to(LazyList)
    .foldLeftM(()) { case (_, epoch) =>
      handleEpoch(epoch, Option.when(epoch == 0)(1), clock, mint, headerValidation, vrfProof, state)
    }
    .void

  private def handleEpoch[F[_]: MonadError[*[_], Throwable]](
    epoch:            Epoch,
    fromSlot:         Option[Slot],
    clock:            ClockAlgebra[F],
    mint:             BlockMintAlgebra[F],
    headerValidation: BlockHeaderValidationAlgebra[F],
    vrfProof:         VrfProofAlgebra[F],
    state:            BlockchainState[F]
  ): F[Unit] =
    clock
      .epochRange(epoch)
      .flatMap(boundary =>
        Monad[F].unit
          .flatTap(_ => println(s"Starting epoch=$epoch (${boundary.start}..${boundary.end})").pure[F])
          .flatTap(_ => println("Precomputing VRF data").pure[F])
          .flatTap(_ =>
            state
              .lookupEta(epoch)
              .flatMap(
                _.fold(new IllegalStateException(s"Unknown Eta for epoch=$epoch").raiseError[F, Unit])(
                  vrfProof.precomputeForEpoch(epoch, _)
                )
              )
              .flatMap(_ =>
                fromSlot
                  .foldLeft(boundary.to(LazyList)) { case (boundary, fromSlot) => boundary.dropWhile(_ < fromSlot) }
                  .foldLeftM(()) { case (_, slot) =>
                    clock
                      .delayedUntilSlot(slot)
                      .flatMap(_ => processSlot(slot, mint, headerValidation, state))
                  }
              )
          )
      )

  private def processSlot[F[_]: Monad](
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
            headerValidation
              .validate(nextBlock.headerV2, canonicalHead.headerV2)
              .flatTap(_ => state.append(nextBlock))
              .flatTap(header => println(s"Appended block ${header.show}").pure[F])
          }
          .value
          .void
      )
}
