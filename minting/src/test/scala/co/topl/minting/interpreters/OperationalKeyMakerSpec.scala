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
import co.topl.crypto.models.SecretKeyKesProduct
import co.topl.crypto.signing._
import co.topl.interpreters.CatsUnsafeResource
import co.topl.minting.algebras.OperationalKeyMakerAlgebra
import co.topl.minting.algebras.VrfCalculatorAlgebra
import co.topl.models.ModelGenerators._
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.models._
import co.topl.models.utility._
import com.google.common.primitives.Longs
import com.google.protobuf.ByteString
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger

import java.util.concurrent.TimeUnit
import scala.collection.immutable.NumericRange
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.util.Random

class OperationalKeyMakerSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  override def munitTimeout: Duration = new FiniteDuration(2, TimeUnit.MINUTES)

  type F[A] = cats.effect.IO[A]

  implicit private val logger: Logger[F] = new NoOpLogger[F]

  implicit private val kesProduct: KesProduct = new KesProduct

  test("load the initial key from SecureStore and produce (VRF-filtered) linear keys") {
    withMock {
      val eta = arbitraryEta.arbitrary.first
      val address = arbitraryStakingAddress.arbitrary.first
      val secureStore = mock[SecureStore[F]]
      val clock = mock[ClockAlgebra[F]]
      val vrfProof = mock[VrfCalculatorAlgebra[F]]
      val etaCalculation = mock[EtaCalculationAlgebra[F]]
      val consensusState = mock[ConsensusValidationStateAlgebra[F]]
      val parentSlotId = SlotId(10L, BlockId.of(ByteString.copyFrom(Array.fill(32)(0: Byte))))
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
        .ineligibleSlots(_: Epoch, _: Eta, _: Option[NumericRange.Exclusive[Long]], _: Ratio))
        .expects(*, *, *, *)
        .once()
        .returning(ineligibilities.pure[F])

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
              parentSlotId,
              operationalPeriodLength,
              activationOperationalPeriod,
              address,
              secureStore,
              clock,
              vrfProof,
              etaCalculation,
              consensusState,
              kesProductResource,
              ed25519Resource
            )

          _ <- Range
            .Long(1, operationalPeriodLength, 2)
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

      (etaCalculation
        .etaToBe(_: SlotId, _: Slot))
        .expects(*, *)
        .once()
        .returning(eta.pure[F])

      (vrfProof
        .ineligibleSlots(_: Epoch, _: Eta, _: Option[NumericRange.Exclusive[Long]], _: Ratio))
        .expects(*, *, *, *)
        .once()
        .returning(Vector.empty[Slot].pure[F])

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
            parentSlotId,
            operationalPeriodLength,
            activationOperationalPeriod,
            address,
            secureStore,
            clock,
            vrfProof,
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
