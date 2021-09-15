package co.topl.fullnode

import cats.Monad
import cats.data.OptionT
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.consensus.KesCertifies.instances._
import co.topl.consensus.KesCertifies.ops._
import co.topl.consensus.LeaderElection
import co.topl.crypto.typeclasses.Evolves.instances._
import co.topl.crypto.typeclasses.Evolves.ops._
import co.topl.crypto.typeclasses.KeyInitializer.Instances._
import co.topl.crypto.typeclasses.{ContainsVerificationKey, KeyInitializer}
import co.topl.minting.BlockMintProgram
import co.topl.models._
import co.topl.models.utility.Ratio

trait Staker[F[_]] {

  def mintBlock(
    head:          BlockV2,
    transactions:  List[Transaction],
    relativeStake: BlockHeaderV2 => TaktikosAddress => F[Option[Ratio]],
    epochNonce:    BlockHeaderV2 => F[Eta]
  ): F[Option[BlockV2]]
}

object Staker {

  object Eval {

    def make[F[_]: Monad](
      stakerAddress:  TaktikosAddress,
      clock:          ClockAlgebra[F],
      leaderElection: LeaderElection[F]
    ): Staker[F] =
      new Staker[F] {

        private val vrfKey =
          KeyInitializer[PrivateKeys.Vrf].random()

        private var currentKesKey: PrivateKeys.Kes = {
          implicit val slot: Slot = 0L
          KeyInitializer[PrivateKeys.Kes].random()
        }

        private def nextKesCertificate(unsignedBlock: BlockHeaderV2.Unsigned): F[KesCertificate] = {
          import ContainsVerificationKey.instances._
          val currentOffset =
            implicitly[ContainsVerificationKey[PrivateKeys.Kes, PublicKeys.Kes]].verificationKeyOf(currentKesKey).offset
          currentKesKey = currentKesKey.evolveSteps(unsignedBlock.slot - currentOffset)
          currentKesKey.certify(unsignedBlock).pure[F]
        }

        implicit private val mint: BlockMintProgram[F] = new BlockMintProgram[F]

        def mintBlock(
          head:          BlockV2,
          transactions:  List[Transaction],
          relativeStake: BlockHeaderV2 => TaktikosAddress => F[Option[Ratio]],
          epochNonce:    BlockHeaderV2 => F[Eta]
        ): F[Option[BlockV2]] = {
          val interpreter = new BlockMintProgram.Algebra[F] {
            def address: F[TaktikosAddress] = stakerAddress.pure[F]

            def unconfirmedTransactions: F[Seq[Transaction]] = (transactions: Seq[Transaction]).pure[F]

            def elect(parent: BlockHeaderV2): F[Option[BlockMintProgram.Election]] =
              OptionT(address.flatMap(relativeStake(parent)(_)))
                .flatMap(relStake =>
                  OptionT(
                    (clock.epochOf(parent.slot).flatMap(clock.epochBoundary), epochNonce(parent)).tupled.flatMap {
                      case (boundary, eta) =>
                        leaderElection.nextHit(vrfKey, relStake, parent.slot, boundary.end, eta)
                    }
                  )
                )
                .map(hit =>
                  BlockMintProgram.Election(
                    slot = hit.slot,
                    hit.cert,
                    hit.threshold
                  )
                )
                .value

            def canonicalHead: F[BlockV2] = head.pure[F]
          }

          OptionT(mint.next(interpreter))
            .semiflatMap(out =>
              clock
                .currentTimestamp()
                .map(out.unsignedHeaderF(_).slot)
                .flatMap(clock.delayedUntilSlot)
                // TODO Check for cancellation
                .flatMap(_ =>
                  clock
                    .currentTimestamp()
                    .flatMap(timestamp =>
                      nextKesCertificate(out.unsignedHeaderF(timestamp)).map(out.signed(_, timestamp))
                    )
                )
            )
            .value
        }
      }
  }
}
