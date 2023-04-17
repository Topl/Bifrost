package co.topl.minting.interpreters

import cats.Applicative
import cats.data.Chain
import cats.effect.IO
import cats.effect.IO.asyncForIO
import cats.effect.implicits.effectResourceOps
import cats.implicits._
import co.topl.algebras._
import co.topl.algebras.testInterpreters.NoOpLogger
import co.topl.codecs.bytes.typeclasses.Persistable
import co.topl.consensus.algebras._
import co.topl.consensus.models._
import co.topl.crypto.hash.Blake2b512
import co.topl.crypto.models.SecretKeyKesProduct
import co.topl.crypto.signing._
import co.topl.interpreters.CatsUnsafeResource
import co.topl.minting.algebras.OperationalKeyMakerAlgebra
import co.topl.minting.algebras.VrfCalculatorAlgebra
import co.topl.models.ModelGenerators._
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.models._
import co.topl.models.utility.HasLength.instances.byteStringLength
import co.topl.models.utility._
import com.google.common.primitives.Longs
import com.google.protobuf.ByteString
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.util.Random

class OperationalKeyMakerSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  override def munitTimeout: Duration = new FiniteDuration(2, TimeUnit.MINUTES)

  type F[A] = cats.effect.IO[A]

  implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromName("OperationalKeyMakerSpec")

  implicit private val kesProduct: KesProduct = new KesProduct

  private val vrfConfig = VrfConfig(15, 40, Ratio(1, 20), Ratio(1, 2))

  test("load the initial key from SecureStore and produce (VRF-filtered) linear keys") {
    withMock {
      val eta = arbitraryEta.arbitrary.first
      val address = arbitraryStakingAddress.arbitrary.first
      val secureStore = mock[SecureStore[F]]
      val clock = mock[ClockAlgebra[F]]
      val vrfProof = mock[VrfCalculatorAlgebra[F]]
      val leaderElection = mock[LeaderElectionValidationAlgebra[F]]
      val etaCalculation = mock[EtaCalculationAlgebra[F]]
      val consensusState = mock[ConsensusValidationStateAlgebra[F]]
      val parentSlotId = SlotId(0L, BlockId.of(ByteString.copyFrom(Array.fill(32)(0: Byte))))
      val operationalPeriodLength = 30L
      val activationOperationalPeriod = 0L
      val (sk, vk) = kesProduct.createKeyPair(Random.nextBytes(32), (2, 2), 0L)

      val ineligibilities = Range.Long(0L, operationalPeriodLength, 2L).toVector

      (() => clock.slotsPerEpoch)
        .expects()
        .once()
        .returning(210L.pure[F])

      (() => secureStore.list)
        .expects()
        .once()
        .returning(Chain("a").pure[F])

      (secureStore
        .consume[SecretKeyKesProduct](_: String)(_: Persistable[SecretKeyKesProduct]))
        .expects("a", *)
        .once()
        .returning(sk.some.pure[F])

      (secureStore
        .write[SecretKeyKesProduct](_: String, _: SecretKeyKesProduct)(_: Persistable[SecretKeyKesProduct]))
        .expects(*, *, *)
        .once()
        .returning(Applicative[F].unit)

      (etaCalculation
        .etaToBe(_: SlotId, _: Slot))
        .expects(*, *)
        .once()
        .returning(eta.pure[F])

      (vrfProof
        .rhoForSlot(_: Slot, _: Eta))
        .expects(*, *)
        .anyNumberOfTimes()
        .onCall { case (slot: Slot, _: Eta) =>
          // Encode the slot as the first 8 bytes of the Rho so that it can be decoded by another mock later
          Rho(Sized.strictUnsafe(ByteString.copyFrom(Longs.toByteArray(slot)).concat(new Array[Byte](64 - 8)))).pure[F]
        }

      (leaderElection
        .getThreshold(_: Ratio, _: Long))
        .expects(*, *)
        .once()
        .returning(Ratio.One.pure[F])

      (leaderElection
        .isSlotLeaderForThreshold(_: Ratio)(_: Rho))
        .expects(*, *)
        .anyNumberOfTimes()
        .onCall { case (_: Ratio, rho: Rho) =>
          // Decode the first 8 bytes of the Rho to determine the slot, and return true if the slot is "odd"
          (Longs.fromByteArray(rho.sizedBytes.data.take(8).toArray) % 2 != 0).pure[F]
        }

      (consensusState
        .operatorRelativeStake(_: BlockId, _: Slot)(_: StakingAddress))
        .expects(*, *, *)
        .once()
        .returning(Ratio.One.some.pure[F])

      val res =
        for {
          kesProductResource <- CatsUnsafeResource.make(new KesProduct, 1).toResource
          ed25519Resource    <- CatsUnsafeResource.make(new Ed25519, 1).toResource
          underTest <-
            OperationalKeyMaker.make[F](
              operationalPeriodLength,
              activationOperationalPeriod,
              address,
              vrfConfig,
              secureStore,
              clock,
              vrfProof,
              leaderElection,
              etaCalculation,
              consensusState,
              kesProductResource,
              ed25519Resource
            )
          // The keys are created in a background fiber, so we need to wait for that fiber to complete before
          // verifying mocks
          _ <- underTest.operationalKeyForSlot(operationalPeriodLength - 1, parentSlotId).toResource

          _ <- Range
            .Long(1, operationalPeriodLength, 1)
            .toVector
            .traverse(slot =>
              if (ineligibilities.contains(slot))
                underTest.operationalKeyForSlot(slot, parentSlotId).assertEquals(None)
              else
                verifyOut(underTest)(vk, slot, parentSlotId)(kesProductResource, ed25519Resource)
            )
            .toResource
        } yield ()
      res.use_
    }
  }

  test("update the initial key at the turn of an operational period") {
    withMock {
      val eta = arbitraryEta.arbitrary.first
      val address = arbitraryStakingAddress.arbitrary.first
      val secureStore = mock[SecureStore[F]]
      val clock = mock[ClockAlgebra[F]]
      val vrfProof = mock[VrfCalculatorAlgebra[F]]
      val etaCalculation = mock[EtaCalculationAlgebra[F]]
      val leaderElection = mock[LeaderElectionValidationAlgebra[F]]
      val consensusState = mock[ConsensusValidationStateAlgebra[F]]
      val parentSlotId = SlotId(10L, BlockId.of(ByteString.copyFrom(Array.fill(32)(0: Byte))))
      val operationalPeriodLength = 30L
      val activationOperationalPeriod = 0L
      val (sk, vk) = kesProduct.createKeyPair(Random.nextBytes(32), (2, 2), 0L)

      (() => clock.slotsPerEpoch)
        .expects()
        .anyNumberOfTimes()
        .returning(210L.pure[F])

      (() => secureStore.list)
        .expects()
        .once()
        .returning(Chain("a").pure[F])

      (secureStore
        .consume[SecretKeyKesProduct](_: String)(_: Persistable[SecretKeyKesProduct]))
        .expects("a", *)
        .once()
        .returning(sk.some.pure[F])

      (secureStore
        .write[SecretKeyKesProduct](_: String, _: SecretKeyKesProduct)(_: Persistable[SecretKeyKesProduct]))
        .expects(*, *, *)
        .once()
        .returning(Applicative[F].unit)

      (vrfProof
        .rhoForSlot(_: Slot, _: Eta))
        .expects(*, *)
        .anyNumberOfTimes()
        .returning(Rho(Sized.strictUnsafe(ByteString.copyFrom(new Array[Byte](64)))).pure[F])

      (leaderElection
        .getThreshold(_: Ratio, _: Long))
        .expects(*, *)
        .once()
        .returning(Ratio.One.pure[F])

      (leaderElection
        .isSlotLeaderForThreshold(_: Ratio)(_: Rho))
        .expects(*, *)
        .anyNumberOfTimes()
        .returning(true.pure[F])

      (etaCalculation
        .etaToBe(_: SlotId, _: Slot))
        .expects(*, *)
        .once()
        .returning(eta.pure[F])

      (consensusState
        .operatorRelativeStake(_: BlockId, _: Slot)(_: StakingAddress))
        .expects(*, *, *)
        .once()
        .returning(Ratio.One.some.pure[F])

      val res = for {
        kesProductResource <- CatsUnsafeResource.make(new KesProduct, 1).toResource
        ed25519Resource    <- CatsUnsafeResource.make(new Ed25519, 1).toResource
        underTest <-
          OperationalKeyMaker.make[F](
            operationalPeriodLength,
            activationOperationalPeriod,
            address,
            vrfConfig,
            secureStore,
            clock,
            vrfProof,
            leaderElection,
            etaCalculation,
            consensusState,
            kesProductResource,
            ed25519Resource
          )
        _ <- Range
          .Long(operationalPeriodLength, operationalPeriodLength * 2, 1)
          .toVector
          .traverse(slot =>
            verifyOut(underTest)(vk.copy(step = 1), slot, parentSlotId)(kesProductResource, ed25519Resource)
          )
          .toResource
      } yield ()
      res.use_
    }
  }

  private def verifyOut(underTest: OperationalKeyMakerAlgebra[F])(
    parentVK:     VerificationKeyKesProduct,
    slot:         Slot,
    parentSlotId: SlotId
  )(kesProductResource: UnsafeResource[F, KesProduct], ed25519Resource: UnsafeResource[F, Ed25519]) =
    for {
      out <- underTest.operationalKeyForSlot(slot, parentSlotId).map(_.get)
      _ = assert(out.slot == slot)
      _ = assert(out.parentVK == parentVK)
      childVK <- ed25519Resource.use(ed => IO.delay(ed.getVerificationKey(Ed25519.SecretKey(out.childSK.toByteArray))))
      childVerificationResult <- kesProductResource.use(kesProduct =>
        IO.delay(
          kesProduct
            .verify(
              out.parentSignature,
              childVK.bytes ++ Longs.toByteArray(slot),
              parentVK
            )
        )
      )
      _ = assert(childVerificationResult)
    } yield ()

}
