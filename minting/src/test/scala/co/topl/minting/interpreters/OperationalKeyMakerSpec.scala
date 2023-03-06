package co.topl.minting.interpreters

import cats.Applicative
import cats.data.Chain
import cats.kernel.Eq
import cats.effect.IO.asyncForIO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.algebras.testInterpreters.NoOpLogger
import co.topl.algebras.{ClockAlgebra, SecureStore}
import co.topl.codecs.bytes.typeclasses.Persistable
import co.topl.consensus.algebras.{ConsensusValidationStateAlgebra, EtaCalculationAlgebra}
import co.topl.consensus.models.CryptoConsensusMorphismInstances._
import co.topl.crypto.signing._
import co.topl.crypto.models.EqInstances._
import co.topl.interpreters.CatsUnsafeResource
import co.topl.minting.algebras.{OperationalKeyMakerAlgebra, VrfCalculatorAlgebra}
import co.topl.models.ModelGenerators._
import co.topl.models._
import co.topl.models.utility._
import co.topl.models.utility.Ratio
import co.topl.consensus.models.{BlockId, SlotId}
import co.topl.crypto.models.SecretKeyKesProduct
import com.google.common.primitives.Longs
import com.google.protobuf.ByteString
import org.scalacheck.Arbitrary
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{EitherValues, OptionValues}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.typelevel.log4cats.Logger

import scala.collection.immutable.NumericRange
import scala.util.Random

class OperationalKeyMakerSpec
    extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with MockFactory
    with EitherValues
    with OptionValues {

  behavior of "OperationalKeys"

  type F[A] = cats.effect.IO[A]

  implicit private val logger: Logger[F] = new NoOpLogger[F]

  implicit private val kesProduct: KesProduct = new KesProduct
  implicit private val ed25519: Ed25519 = new Ed25519

  implicit val arbitraryStakingAddress: Arbitrary[StakingAddress] = Arbitrary(stakingAddressGen)

  it should "load the initial key from SecureStore and produce (VRF-filtered) linear keys" in {
    forAll { (eta: Eta, address: StakingAddress) =>
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

      val keyMakerF =
        for {
          kesProductResource <- CatsUnsafeResource.make(new KesProduct, 1)
          ed25519Resource    <- CatsUnsafeResource.make(new Ed25519, 1)
          keyMaker <-
            OperationalKeyMaker.make[F](
              0L,
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
        } yield keyMaker

      val underTest: OperationalKeyMakerAlgebra[F] = keyMakerF.unsafeRunSync()

      ineligibilities.foreach { i =>
        val out = underTest.operationalKeyForSlot(i, parentSlotId).unsafeRunSync()
        out shouldBe None
      }

      Range.Long(1, operationalPeriodLength, 2).foreach { i =>
        val out = underTest.operationalKeyForSlot(i, parentSlotId).unsafeRunSync().value
        out.slot shouldBe i
        Eq.eqv(
          out.parentVK.toF[F, co.topl.crypto.models.VerificationKeyKesProduct].unsafeRunSync().value,
          vk
        ) shouldBe true
        val parentSignature =
          out.parentSignature.toF[F, co.topl.crypto.models.SignatureKesProduct].unsafeRunSync().value
        kesProduct
          .verify(
            parentSignature,
            ed25519.getVerificationKey(out.childSK).toArray ++ Longs.toByteArray(i),
            vk
          )
      }
    }
  }

  it should "update the initial key at the turn of an operational period" in {
    forAll { (eta: Eta, address: StakingAddress) =>
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
        .anyNumberOfTimes()
        .returning(eta.pure[F])

      (vrfProof
        .ineligibleSlots(_: Epoch, _: Eta, _: Option[NumericRange.Exclusive[Long]], _: Ratio))
        .expects(*, *, *, *)
        .anyNumberOfTimes()
        .returning(Vector.empty[Slot].pure[F])

      (consensusState
        .operatorRelativeStake(_: BlockId, _: Slot)(_: StakingAddress))
        .expects(*, *, *)
        .twice()
        .returning(Ratio.One.some.pure[F])

      val keyMakerAlgebraF =
        for {
          kesProductResource <- CatsUnsafeResource.make(new KesProduct, 1)
          ed25519Resource    <- CatsUnsafeResource.make(new Ed25519, 1)
          keyMaker <-
            OperationalKeyMaker.make[F](
              0L,
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
        } yield keyMaker

      val underTest: OperationalKeyMakerAlgebra[F] = keyMakerAlgebraF.unsafeRunSync()

      (() => secureStore.list)
        .expects()
        .once()
        .returning(Chain("b").pure[F])

      (secureStore
        .consume[SecretKeyKesProduct](_: String)(_: Persistable[SecretKeyKesProduct]))
        .expects("b", *)
        .once()
        .returning(kesProduct.update(sk, 1).some.pure[F])

      (secureStore
        .write[SecretKeyKesProduct](_: String, _: SecretKeyKesProduct)(_: Persistable[SecretKeyKesProduct]))
        .expects(*, *, *)
        .once()
        .returning(Applicative[F].unit)

      Range.Long(operationalPeriodLength, operationalPeriodLength * 2, 1).foreach { i =>
        val out = underTest.operationalKeyForSlot(i, parentSlotId).unsafeRunSync().value
        out.slot shouldBe i
        Eq.eqv(
          out.parentVK.toF[F, co.topl.crypto.models.VerificationKeyKesProduct].unsafeRunSync().value,
          vk.copy(step = 1)
        ) shouldBe true
        val parentSignature =
          out.parentSignature.toF[F, co.topl.crypto.models.SignatureKesProduct].unsafeRunSync().value
        kesProduct
          .verify(
            parentSignature,
            ed25519.getVerificationKey(out.childSK).toArray ++ Longs.toByteArray(i),
            vk.copy(step = 1)
          )
      }
    }
  }

}
