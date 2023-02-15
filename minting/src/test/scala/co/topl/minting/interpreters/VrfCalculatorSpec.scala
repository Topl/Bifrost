package co.topl.minting.interpreters

import cats.effect.IO
import cats.effect.IO.asyncForIO
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.consensus.algebras.LeaderElectionValidationAlgebra
import co.topl.consensus.models.VrfConfig
import co.topl.crypto.signing.Ed25519VRF
import co.topl.interpreters.CatsUnsafeResource
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Ratio, Sized}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import scodec.bits._

class VrfCalculatorSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  // test the cache implementation is not possible in the current implementation,
  // Time to live is not used on testing for both vrfProofs and rhos caches
  val vrfCacheTtl = 0L

  test("proofForSlot: fixed input") {
    for {
      ed25519Resource <- CatsUnsafeResource.make(new Ed25519VRF, 1)
      vrfCalculator <- VrfCalculator.make[F](
        skVrf = SecretKeys.VrfEd25519(Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0)))),
        clock = null,
        leaderElectionValidation = null,
        ed25519Resource,
        vrfConfig = null,
        vrfCacheTtl
      )

      slot = 10L
      eta = Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0))): Eta

      expectedProof = Proofs.Knowledge.VrfEd25519(
        Sized.strictUnsafe(
          hex"bc31a2fb46995ffbe4b316176407f57378e2f3d7fee57d228a811194361d8e7040c9d15575d7a2e75506ffe1a47d772168b071a99d2e85511730e9c21397a1cea0e7fa4bd161e6d5185a94a665dd190d"
        )
      )

      _ <- vrfCalculator.proofForSlot(slot, eta).assertEquals(expectedProof)
    } yield ()
  }

  test("rhoForSlot: fixed input") {
    for {
      ed25519Resource <- CatsUnsafeResource.make(new Ed25519VRF, 1)
      vrfCalculator <- VrfCalculator.make[F](
        skVrf = SecretKeys.VrfEd25519(Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0)))),
        clock = null,
        leaderElectionValidation = null,
        ed25519Resource,
        vrfConfig = null,
        vrfCacheTtl
      )

      slot = 10L
      eta = Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0))): Eta

      expectedRho = Rho(
        Sized.strictUnsafe(
          data =
            hex"c30d2304d5d76e7cee8cc0eb66493528cc9e5a9cc03449bc8ed3dab192ba1e8edb3567b4ffc63526c69a6d05a73b57879529ccf8dd22e596080257843748d569"
        )
      )

      _ <- vrfCalculator.rhoForSlot(slot, eta).assertEquals(expectedRho)
    } yield ()
  }

  test("ineligibleSlots: in empty inRange, no slots leader") {
    withMock {
      val epoch = 1L
      val eta = Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0))): Eta
      val relativeStake = Ratio.One
      val vrfConfig =
        VrfConfig(lddCutoff = 0, precision = 16, baselineDifficulty = Ratio(1, 15), amplitude = Ratio(2, 5))

      val clock = mock[ClockAlgebra[F]]
      val leaderElectionValidation = mock[LeaderElectionValidationAlgebra[F]]

      (() => clock.slotsPerEpoch).expects().once().returning(10L.pure[F])

      (leaderElectionValidation.getThreshold _)
        .expects(relativeStake, vrfConfig.lddCutoff)
        .once()
        .returning(Ratio.One.pure[F])

      (leaderElectionValidation
        .isSlotLeaderForThreshold(_: Ratio)(_: Rho))
        .expects(relativeStake, *)
        .anyNumberOfTimes()
        .returning(false.pure[F])

      for {
        ed25519Resource <- CatsUnsafeResource.make(new Ed25519VRF, 1)
        vrfCalculator <- VrfCalculator.make[F](
          skVrf = SecretKeys.VrfEd25519(Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0)))),
          clock,
          leaderElectionValidation,
          ed25519Resource,
          vrfConfig,
          vrfCacheTtl
        )

        expectedSlot: Vector[Slot] = Vector(10, 11, 12, 13, 14, 15, 16, 17, 18, 19)

        _ <- vrfCalculator.ineligibleSlots(epoch, eta, inRange = None, relativeStake).assertEquals(expectedSlot)
      } yield ()

    }
  }

  test("ineligibleSlots: in empty inRange, all slots leader") {
    withMock {
      val epoch = 1L
      val eta = Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0))): Eta
      val relativeStake = Ratio.One
      val vrfConfig =
        VrfConfig(lddCutoff = 0, precision = 16, baselineDifficulty = Ratio(1, 15), amplitude = Ratio(2, 5))

      val clock = mock[ClockAlgebra[F]]
      val leaderElectionValidation = mock[LeaderElectionValidationAlgebra[F]]

      (() => clock.slotsPerEpoch).expects().once().returning(10L.pure[F])

      (leaderElectionValidation.getThreshold _)
        .expects(relativeStake, vrfConfig.lddCutoff)
        .once()
        .returning(Ratio.One.pure[F])

      (leaderElectionValidation
        .isSlotLeaderForThreshold(_: Ratio)(_: Rho))
        .expects(relativeStake, *)
        .anyNumberOfTimes()
        .returning(true.pure[F])

      for {
        ed25519Resource <- CatsUnsafeResource.make(new Ed25519VRF, 1)
        vrfCalculator <- VrfCalculator.make[F](
          skVrf = SecretKeys.VrfEd25519(Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0)))),
          clock,
          leaderElectionValidation,
          ed25519Resource,
          vrfConfig,
          vrfCacheTtl
        )

        _ <- vrfCalculator.ineligibleSlots(epoch, eta, inRange = None, relativeStake).assertEquals(Vector.empty[Slot])
      } yield ()

    }
  }

}
