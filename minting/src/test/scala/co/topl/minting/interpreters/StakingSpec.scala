package co.topl.minting.interpreters

import cats.effect.IO
import cats.effect.IO.asyncForIO
import cats.implicits._
import co.topl.consensus.algebras.{
  ConsensusValidationStateAlgebra,
  EtaCalculationAlgebra,
  LeaderElectionValidationAlgebra
}
import co.topl.consensus.models.{BlockId, SlotId}
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.minting.algebras.{OperationalKeyMakerAlgebra, VrfCalculatorAlgebra}
import co.topl.minting.models.VrfHit
import co.topl.models._
import co.topl.models.utility._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Ratio, Sized}
import co.topl.typeclasses.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import scodec.bits._

class StakingSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  test("elect: with fixed values") {
    PropF.forAllF { (blockId: BlockId) =>
      withMock {
        val slot = 1L
        val parentSlotId = SlotId.of(slot, blockId)
        val eta = Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0))): Eta
        val relativeStake = Ratio.One
        val address =
          StakingAddresses.Operator(
            VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0))))
          )

        val vkVrf = VerificationKeys.VrfEd25519(Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0))))
        val proof = Proofs.Knowledge.VrfEd25519(
          Sized.strictUnsafe(
            hex"bc31a2fb46995ffbe4b316176407f57378e2f3d7fee57d228a811194361d8e7040c9d15575d7a2e75506ffe1a47d772168b071a99d2e85511730e9c21397a1cea0e7fa4bd161e6d5185a94a665dd190d"
          )
        )
        val rho = Rho(
          Sized.strictUnsafe(
            data =
              hex"c30d2304d5d76e7cee8cc0eb66493528cc9e5a9cc03449bc8ed3dab192ba1e8edb3567b4ffc63526c69a6d05a73b57879529ccf8dd22e596080257843748d569"
          )
        )

        val etaCalculation = mock[EtaCalculationAlgebra[F]]
        val consensusState = mock[ConsensusValidationStateAlgebra[F]]
        val leaderElectionValidation = mock[LeaderElectionValidationAlgebra[F]]
        val vrfCalculator = mock[VrfCalculatorAlgebra[F]]

        (etaCalculation.etaToBe _)
          .expects(parentSlotId, slot)
          .once()
          .returning(eta.pure[F])

        (consensusState
          .operatorRelativeStake(_: TypedIdentifier, _: Slot)(_: StakingAddresses.Operator))
          .expects(blockId: TypedIdentifier, slot, address)
          .once()
          .returning(relativeStake.some.pure[F])

        (leaderElectionValidation.getThreshold _)
          .expects(relativeStake, slot - parentSlotId.slot)
          .once()
          .returning(Ratio.One.pure[F])

        (leaderElectionValidation
          .isSlotLeaderForThreshold(_: Ratio)(_: Rho))
          .expects(relativeStake, *)
          .once()
          .returning(true.pure[F])

        (vrfCalculator.proofForSlot _)
          .expects(slot, eta)
          .twice()
          .returning(proof.pure[F])

        (vrfCalculator.rhoForSlot _)
          .expects(slot, eta)
          .once()
          .returning(rho.pure[F])

        for {
          staking <- Staking
            .make[F](
              a = address,
              vkVrf,
              operationalKeyMaker = null,
              consensusState,
              etaCalculation,
              ed25519Resource = null,
              vrfCalculator,
              leaderElectionValidation
            )
            .pure[F]

          testProof <- vrfCalculator.proofForSlot(slot, eta)

          expectedVrfHit = VrfHit(
            EligibilityCertificate(testProof, vkVrf, relativeStake.typedEvidence.evidence, eta),
            slot,
            relativeStake
          )
          _ <- staking.elect(parentSlotId, slot).assertEquals(expectedVrfHit.some)
        } yield ()
      }
    }
  }

  test("certifyBlock: with empty operationalKeyForSlot") {
    PropF.forAllF { (parentSlotId: SlotId, slot: Slot) =>
      withMock {
        val operationalKeyMaker = mock[OperationalKeyMakerAlgebra[F]]
        (operationalKeyMaker.operationalKeyForSlot _)
          .expects(slot, parentSlotId)
          .once()
          .returning(None.pure[F])

        for {
          staking <- Staking
            .make[F](
              a = null,
              vkVrf = null,
              operationalKeyMaker,
              consensusState = null,
              etaCalculation = null,
              ed25519Resource = null,
              vrfCalculator = null,
              leaderElectionValidation = null
            )
            .pure[F]
          _ <- staking
            .certifyBlock(parentSlotId, slot, _ => throw new NotImplementedError("unsignedBlockBuilder"))
            .assertEquals(None)
        } yield ()
      }
    }
  }

  test("getHit: with fixed values") {
    withMock {
      val slot = 1L
      val slotDiff = 1L
      val eta = Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0))): Eta
      val relativeStake = Ratio.One
      val vkVrf = VerificationKeys.VrfEd25519(Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0))))
      val proof = Proofs.Knowledge.VrfEd25519(
        Sized.strictUnsafe(
          hex"bc31a2fb46995ffbe4b316176407f57378e2f3d7fee57d228a811194361d8e7040c9d15575d7a2e75506ffe1a47d772168b071a99d2e85511730e9c21397a1cea0e7fa4bd161e6d5185a94a665dd190d"
        )
      )
      val rho = Rho(
        Sized.strictUnsafe(
          data =
            hex"c30d2304d5d76e7cee8cc0eb66493528cc9e5a9cc03449bc8ed3dab192ba1e8edb3567b4ffc63526c69a6d05a73b57879529ccf8dd22e596080257843748d569"
        )
      )

      val leaderElectionValidation = mock[LeaderElectionValidationAlgebra[F]]
      val vrfCalculator = mock[VrfCalculatorAlgebra[F]]

      (leaderElectionValidation.getThreshold _)
        .expects(relativeStake, slotDiff)
        .once()
        .returning(Ratio.One.pure[F])

      (leaderElectionValidation
        .isSlotLeaderForThreshold(_: Ratio)(_: Rho))
        .expects(relativeStake, *)
        .once()
        .returning(true.pure[F])

      (vrfCalculator.proofForSlot _)
        .expects(slot, eta)
        .twice()
        .returning(proof.pure[F])

      (vrfCalculator.rhoForSlot _)
        .expects(slot, eta)
        .once()
        .returning(rho.pure[F])

      for {
        staking <- Staking
          .make[F](
            a = null,
            vkVrf,
            operationalKeyMaker = null,
            consensusState = null,
            etaCalculation = null,
            ed25519Resource = null,
            vrfCalculator,
            leaderElectionValidation
          )
          .pure[F]

        testProof <- vrfCalculator.proofForSlot(slot, eta)

        expectedVrfHit = VrfHit(
          EligibilityCertificate(testProof, vkVrf, relativeStake.typedEvidence.evidence, eta),
          slot,
          relativeStake
        )
        _ <- staking.getHit(relativeStake, slot, slotDiff, eta).assertEquals(expectedVrfHit.some)
      } yield ()

    }
  }

}
