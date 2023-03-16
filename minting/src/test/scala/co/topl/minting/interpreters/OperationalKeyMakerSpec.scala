package co.topl.minting.interpreters

import cats.Applicative
import cats.data.Chain
import cats.effect.IO.asyncForIO
import cats.effect.implicits.effectResourceOps
import cats.implicits._
import co.topl.algebras._
import co.topl.algebras.testInterpreters.NoOpLogger
import co.topl.codecs.bytes.typeclasses.Persistable
import co.topl.consensus.algebras._
import co.topl.consensus.models.CryptoConsensusMorphismInstances.signatureKesProductIsomorphism
import co.topl.consensus.models.CryptoConsensusMorphismInstances.verificationKeyKesProductIsomorphism
import co.topl.consensus.models._
import co.topl.crypto.models.SecretKeyKesProduct
import co.topl.crypto.signing._
import co.topl.interpreters.CatsUnsafeResource
import co.topl.minting.algebras.VrfCalculatorAlgebra
import co.topl.models.ModelGenerators._
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.models._
import co.topl.models.utility._
import com.google.common.primitives.Longs
import com.google.protobuf.ByteString
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import org.scalatest.OptionValues
import org.typelevel.log4cats.Logger

import java.util.concurrent.TimeUnit
import scala.collection.immutable.NumericRange
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.util.Random

class OperationalKeyMakerSpec
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with OptionValues {

  override def munitTimeout: Duration = new FiniteDuration(2, TimeUnit.MINUTES)

  type F[A] = cats.effect.IO[A]

  implicit private val logger: Logger[F] = new NoOpLogger[F]

  implicit private val kesProduct: KesProduct = new KesProduct
  implicit private val ed25519: Ed25519 = new Ed25519

  test("load the initial key from SecureStore and produce (VRF-filtered) linear keys") {
    PropF.forAllF { (eta: Eta, address: StakingAddress) =>
      withMock {
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

        (() => clock.globalSlot)
          .expects()
          .once()
          .returning(0L.pure[F])

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

        val keyMakerR =
          for {
            kesProductResource <- CatsUnsafeResource.make(new KesProduct, 1).toResource
            ed25519Resource    <- CatsUnsafeResource.make(new Ed25519, 1).toResource
            keyMaker <-
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
          } yield keyMaker

        val res = for {
          underTest <- keyMakerR
          _ <- ineligibilities
            .foreach { i =>
              underTest.operationalKeyForSlot(i, parentSlotId).assertEquals(None)
            }
            .pure[F]
            .toResource

          _ <- Range
            .Long(1, operationalPeriodLength, 2)
            .toVector
            .traverse(i =>
              for {
                out <- underTest.operationalKeyForSlot(i, parentSlotId).map(_.value)
                _   <- assertIO(out.slot.pure[F], i)
                _ <- assertIO(
                  out.parentVK.toF[F, co.topl.crypto.models.VerificationKeyKesProduct].map(_.toOption.value),
                  vk
                )
                parentSignature <- out.parentSignature
                  .toF[F, co.topl.crypto.models.SignatureKesProduct]
                  .map(_.toOption.value)
                _ <- assertIO(
                  kesProduct
                    .verify(
                      parentSignature,
                      ed25519.getVerificationKey(out.childSK: Bytes).toArray ++ Longs.toByteArray(i),
                      vk
                    )
                    .pure[F],
                  true
                )
              } yield ()
            )
            .toResource
        } yield ()
        res.use_
      }
    }
  }

  test("update the initial key at the turn of an operational period") {
    PropF.forAllF { (eta: Eta, address: StakingAddress) =>
      withMock {
        val secureStore = mock[SecureStore[F]]
        val clock = mock[ClockAlgebra[F]]
        val vrfProof = mock[VrfCalculatorAlgebra[F]]
        val etaCalculation = mock[EtaCalculationAlgebra[F]]
        val consensusState = mock[ConsensusValidationStateAlgebra[F]]
        val parentSlotId = SlotId(10L, BlockId.of(ByteString.copyFrom(Array.fill(32)(0: Byte))))
        val operationalPeriodLength = 30L
        val activationOperationalPeriod = 0L
        val (sk, vk) = kesProduct.createKeyPair(Random.nextBytes(32), (2, 2), 0L)

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

        val keyMakerAlgebraR =
          for {
            kesProductResource <- CatsUnsafeResource.make(new KesProduct, 1).toResource
            ed25519Resource    <- CatsUnsafeResource.make(new Ed25519, 1).toResource
            keyMaker <-
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
          } yield keyMaker

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

        val res = for {
          underTest <- keyMakerAlgebraR
          _ <- Range
            .Long(operationalPeriodLength, operationalPeriodLength * 2, 1)
            .toVector
            .traverse(i =>
              for {
                out <- underTest.operationalKeyForSlot(i, parentSlotId).map(_.value)
                _   <- assertIO(out.slot.pure[F], i)
                expectedVK = vk.copy(step = 1)
                _ <- assertIO(
                  out.parentVK.toF[F, co.topl.crypto.models.VerificationKeyKesProduct].map(_.toOption.value),
                  expectedVK
                )
                parentSignature <- out.parentSignature
                  .toF[F, co.topl.crypto.models.SignatureKesProduct]
                  .map(_.toOption.value)
                _ <- assertIO(
                  kesProduct
                    .verify(
                      parentSignature,
                      ed25519.getVerificationKey(out.childSK: Bytes).toArray ++ Longs.toByteArray(i),
                      expectedVK
                    )
                    .pure[F],
                  true
                )
              } yield ()
            )
            .toResource
        } yield ()
        res.use_
      }
    }
  }

}
