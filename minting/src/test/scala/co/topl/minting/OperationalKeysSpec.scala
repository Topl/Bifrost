package co.topl.minting

import cats.data.Chain
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.algebras.{ClockAlgebra, ConsensusState}
import co.topl.consensus.algebras.EtaCalculationAlgebra
import co.topl.crypto.keyfile.SecureStore
import co.topl.crypto.signing.{Ed25519, KesProduct}
import co.topl.minting.algebras.VrfProofAlgebra
import co.topl.models._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{EitherValues, OptionValues}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import ModelGenerators._
import cats.Applicative
import co.topl.codecs.bytes.ByteCodec
import co.topl.models.utility.Ratio
import com.google.common.primitives.Longs

import scala.collection.immutable.NumericRange
import scala.util.Random

class OperationalKeysSpec
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
    forAll { (eta: Eta, address: TaktikosAddress) =>
      val secureStore = mock[SecureStore[F]]
      val clock = mock[ClockAlgebra[F]]
      val vrfProof = mock[VrfProofAlgebra[F]]
      val etaCalculation = mock[EtaCalculationAlgebra[F]]
      val consensusState = mock[ConsensusState[F]]
      val parentSlotId = SlotId(10L, TypedBytes(1: Byte, Bytes.fill(32)(0: Byte)))
      val operationalPeriodLength = 30L
      val activationOperationalPeriod = 0L
      val (sk, vk) = kesProduct.createKeyPair(Bytes(Random.nextBytes(32)), (2, 2), 0L)

      val ineligibilities = Range.Long(0L, operationalPeriodLength, 2L).toVector

      (() => clock.globalSlot)
        .expects()
        .once()
        .returning(0L.pure[F])

      (() => clock.slotsPerEpoch)
        .expects()
        .twice()
        .returning(210L.pure[F])

      (() => secureStore.list)
        .expects()
        .once()
        .returning(Chain("a").pure[F])

      (secureStore
        .consume[SecretKeys.KesProduct](_: String)(_: ByteCodec[SecretKeys.KesProduct]))
        .expects("a", *)
        .once()
        .returning(sk.some.pure[F])

      (secureStore
        .write[SecretKeys.KesProduct](_: String, _: SecretKeys.KesProduct)(_: ByteCodec[SecretKeys.KesProduct]))
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
        .lookupRelativeStake(_: Epoch)(_: TaktikosAddress))
        .expects(*, *)
        .once()
        .returning(Ratio(1).some.pure[F])

      val underTest = OperationalKeys.FromSecureStore
        .make[F](
          secureStore,
          clock,
          vrfProof,
          etaCalculation,
          consensusState,
          parentSlotId,
          operationalPeriodLength,
          activationOperationalPeriod,
          address
        )
        .unsafeRunSync()

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
            out.parentSignature,
            ed25519.getVerificationKey(out.childSK).bytes.data ++ Bytes(Longs.toByteArray(i)),
            vk
          )
      }
    }
  }

  it should "update the initial key at the turn of an operational period" in {
    forAll { (eta: Eta, address: TaktikosAddress) =>
      val secureStore = mock[SecureStore[F]]
      val clock = mock[ClockAlgebra[F]]
      val vrfProof = mock[VrfProofAlgebra[F]]
      val etaCalculation = mock[EtaCalculationAlgebra[F]]
      val consensusState = mock[ConsensusState[F]]
      val parentSlotId = SlotId(10L, TypedBytes(1: Byte, Bytes.fill(32)(0: Byte)))
      val operationalPeriodLength = 30L
      val activationOperationalPeriod = 0L
      val (sk, vk) = kesProduct.createKeyPair(Bytes(Random.nextBytes(32)), (2, 2), 0L)

      (() => clock.globalSlot)
        .expects()
        .once()
        .returning(0L.pure[F])

      (() => clock.slotsPerEpoch)
        .expects()
        .anyNumberOfTimes()
        .returning(210L.pure[F])

      (() => secureStore.list)
        .expects()
        .once()
        .returning(Chain("a").pure[F])

      (secureStore
        .consume[SecretKeys.KesProduct](_: String)(_: ByteCodec[SecretKeys.KesProduct]))
        .expects("a", *)
        .once()
        .returning(sk.some.pure[F])

      (secureStore
        .write[SecretKeys.KesProduct](_: String, _: SecretKeys.KesProduct)(_: ByteCodec[SecretKeys.KesProduct]))
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
        .lookupRelativeStake(_: Epoch)(_: TaktikosAddress))
        .expects(*, *)
        .twice()
        .returning(Ratio(1).some.pure[F])

      val underTest = OperationalKeys.FromSecureStore
        .make[F](
          secureStore,
          clock,
          vrfProof,
          etaCalculation,
          consensusState,
          parentSlotId,
          operationalPeriodLength,
          activationOperationalPeriod,
          address
        )
        .unsafeRunSync()

      (() => secureStore.list)
        .expects()
        .once()
        .returning(Chain("b").pure[F])

      (secureStore
        .consume[SecretKeys.KesProduct](_: String)(_: ByteCodec[SecretKeys.KesProduct]))
        .expects("b", *)
        .once()
        .returning(kesProduct.update(sk, 1).some.pure[F])

      (secureStore
        .write[SecretKeys.KesProduct](_: String, _: SecretKeys.KesProduct)(_: ByteCodec[SecretKeys.KesProduct]))
        .expects(*, *, *)
        .once()
        .returning(Applicative[F].unit)

      Range.Long(operationalPeriodLength, operationalPeriodLength * 2, 1).foreach { i =>
        val out = underTest.operationalKeyForSlot(i, parentSlotId).unsafeRunSync().value
        out.slot shouldBe i
        out.parentVK shouldBe vk.copy(step = 1)
        kesProduct
          .verify(
            out.parentSignature,
            ed25519.getVerificationKey(out.childSK).bytes.data ++ Bytes(Longs.toByteArray(i)),
            vk.copy(step = 1)
          )
      }
    }
  }

}

class NoOpLogger[F[_]: Applicative] extends Logger[F] {
  def error(t: Throwable)(message: => String): F[Unit] = Applicative[F].unit

  def warn(t: Throwable)(message: => String): F[Unit] = Applicative[F].unit

  def info(t: Throwable)(message: => String): F[Unit] = Applicative[F].unit

  def debug(t: Throwable)(message: => String): F[Unit] = Applicative[F].unit

  def trace(t: Throwable)(message: => String): F[Unit] = Applicative[F].unit

  def error(message: => String): F[Unit] = Applicative[F].unit

  def warn(message: => String): F[Unit] = Applicative[F].unit

  def info(message: => String): F[Unit] = Applicative[F].unit

  def debug(message: => String): F[Unit] = Applicative[F].unit

  def trace(message: => String): F[Unit] = Applicative[F].unit
}
