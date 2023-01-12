package co.topl.minting.interpreters

import cats.Applicative
import cats.data.Chain
import cats.effect.IO.asyncForIO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.algebras.testInterpreters.NoOpLogger
import co.topl.algebras.{ClockAlgebra, SecureStore}
import co.topl.codecs.bytes.typeclasses.Persistable
import co.topl.consensus.algebras.{ConsensusValidationStateAlgebra, EtaCalculationAlgebra}
import co.topl.crypto.signing._
import co.topl.interpreters.CatsUnsafeResource
import co.topl.minting.algebras.{OperationalKeyMakerAlgebra, VrfCalculatorAlgebra}
import co.topl.models.ModelGenerators._
import co.topl.models._
import co.topl.models.utility.{Ratio, ReplaceModelUtil}
import com.google.common.primitives.Longs
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

  it should "load the initial key from SecureStore and produce (VRF-filtered) linear keys" in {
    forAll { (eta: Eta, address: StakingAddresses.Operator) =>
      val secureStore = mock[SecureStore[F]]
      val clock = mock[ClockAlgebra[F]]
      val vrfProof = mock[VrfCalculatorAlgebra[F]]
      val etaCalculation = mock[EtaCalculationAlgebra[F]]
      val consensusState = mock[ConsensusValidationStateAlgebra[F]]
      val parentSlotId = SlotId(10L, TypedBytes(1: Byte, Bytes.fill(32)(0: Byte)))
      val operationalPeriodLength = 30L
      val activationOperationalPeriod = 0L
      val (sk, vk) = kesProduct.createKeyPair(Bytes(Random.nextBytes(32)), (2, 2), 0L)

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
        .consume[SecretKeys.KesProduct](_: String)(_: Persistable[SecretKeys.KesProduct]))
        .expects("a", *)
        .once()
        .returning(sk.some.pure[F])

      (secureStore
        .write[SecretKeys.KesProduct](_: String, _: SecretKeys.KesProduct)(_: Persistable[SecretKeys.KesProduct]))
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
        .operatorRelativeStake(_: TypedIdentifier, _: Slot)(_: StakingAddresses.Operator))
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
        out.parentVK shouldBe vk
        kesProduct
          .verify(
            ReplaceModelUtil.signatureKesProduct(out.parentSignature),
            ed25519.getVerificationKey(out.childSK.bytes.data) ++ Bytes(Longs.toByteArray(i)),
            ReplaceModelUtil.verificationKeyKesProduct(vk)
          )
      }
    }
  }

  it should "update the initial key at the turn of an operational period" in {
    forAll { (eta: Eta, address: StakingAddresses.Operator) =>
      val secureStore = mock[SecureStore[F]]
      val clock = mock[ClockAlgebra[F]]
      val vrfProof = mock[VrfCalculatorAlgebra[F]]
      val etaCalculation = mock[EtaCalculationAlgebra[F]]
      val consensusState = mock[ConsensusValidationStateAlgebra[F]]
      val parentSlotId = SlotId(10L, TypedBytes(1: Byte, Bytes.fill(32)(0: Byte)))
      val operationalPeriodLength = 30L
      val activationOperationalPeriod = 0L
      val (sk, vk) = kesProduct.createKeyPair(Bytes(Random.nextBytes(32)), (2, 2), 0L)

      (() => clock.slotsPerEpoch)
        .expects()
        .anyNumberOfTimes()
        .returning(210L.pure[F])

      (() => secureStore.list)
        .expects()
        .once()
        .returning(Chain("a").pure[F])

      (secureStore
        .consume[SecretKeys.KesProduct](_: String)(_: Persistable[SecretKeys.KesProduct]))
        .expects("a", *)
        .once()
        .returning(sk.some.pure[F])

      (secureStore
        .write[SecretKeys.KesProduct](_: String, _: SecretKeys.KesProduct)(_: Persistable[SecretKeys.KesProduct]))
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
        .operatorRelativeStake(_: TypedIdentifier, _: Slot)(_: StakingAddresses.Operator))
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
        .consume[SecretKeys.KesProduct](_: String)(_: Persistable[SecretKeys.KesProduct]))
        .expects("b", *)
        .once()
        .returning(kesProduct.update(sk, 1).some.pure[F])

      (secureStore
        .write[SecretKeys.KesProduct](_: String, _: SecretKeys.KesProduct)(_: Persistable[SecretKeys.KesProduct]))
        .expects(*, *, *)
        .once()
        .returning(Applicative[F].unit)

      Range.Long(operationalPeriodLength, operationalPeriodLength * 2, 1).foreach { i =>
        val out = underTest.operationalKeyForSlot(i, parentSlotId).unsafeRunSync().value
        out.slot shouldBe i
        out.parentVK shouldBe vk.copy(step = 1)
        kesProduct
          .verify(
            ReplaceModelUtil.signatureKesProduct(out.parentSignature),
            ed25519.getVerificationKey(out.childSK.bytes.data) ++ Bytes(Longs.toByteArray(i)),
            ReplaceModelUtil.verificationKeyKesProduct(vk.copy(step = 1))
          )
      }
    }
  }

}
