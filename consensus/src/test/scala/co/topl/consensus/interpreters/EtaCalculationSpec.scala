package co.topl.consensus.interpreters

import cats.data.NonEmptyChain
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.algebras.{ClockAlgebra, UnsafeResource}
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.{BlockId, SlotData, SlotId, VrfArgument}
import co.topl.consensus.rhoToRhoNonceHash
import co.topl.crypto.signing.Ed25519VRF
import co.topl.crypto.hash.{Blake2b256, Blake2b512}
import co.topl.models.ModelGenerators._
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.models._
import co.topl.models.utility.HasLength.instances.byteStringLength
import co.topl.models.utility._
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scodec.bits.ByteVector

class EtaCalculationSpec
    extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with MockFactory
    with EitherValues {

  behavior of "EtaCalculation"

  type F[A] = IO[A]

  implicit private val ed25519Vrf: Ed25519VRF =
    Ed25519VRF.precomputed()

  it should "compute the eta for an epoch" in {
    val clock = mock[ClockAlgebra[F]]
    val blake2b256Resource = mock[UnsafeResource[F, Blake2b256]]
    val blake2b512Resource = mock[UnsafeResource[F, Blake2b512]]
    val bigBangHeader = arbitraryHeader.arbitrary.first.copy(slot = 0L, parentSlot = -1L)

    (() => clock.slotsPerEpoch)
      .expects()
      .anyNumberOfTimes()
      .returning(15L.pure[F])

    val fetchSlotData = mockFunction[BlockId, F[SlotData]]

    val underTest =
      EtaCalculation
        .make[F](
          fetchSlotData,
          clock,
          Sized.strictUnsafe(bigBangHeader.eligibilityCertificate.eta),
          blake2b256Resource,
          blake2b512Resource
        )
        .unsafeRunSync()
    val epoch = 0L
    val (skVrf, _) = ed25519Vrf.generateRandom
    val args: List[(Slot, ByteString)] = List.tabulate(8) { offset =>
      val slot = offset.toLong + 1
      val signature =
        ed25519Vrf.sign(
          skVrf,
          VrfArgument(Sized.strictUnsafe(bigBangHeader.eligibilityCertificate.eta), slot).signableBytes
        )
      slot -> signature
    }

    val blocks: List[BlockHeader] =
      bigBangHeader ::
      LazyList
        .unfold(List(bigBangHeader)) {
          case items if items.length == args.length + 1 => None
          case items =>
            val (slot, signature) = args(items.length - 1)
            val nextHeader = headerGen(
              slotGen = Gen.const[Long](slot),
              parentSlotGen = Gen.const(items.last.slot),
              eligibilityCertificateGen = arbitraryEligibilityCertificate.arbitrary.map(c =>
                c.copy(vrfSig = signature, eta = bigBangHeader.eligibilityCertificate.eta)
              ),
              parentHeaderIdGen = Gen.const(items.last.id)
            ).first
            (nextHeader -> (items :+ nextHeader)).some
        }
        .toList

    fetchSlotData
      .expects(*)
      .onCall((id: BlockId) =>
        blocks
          .find(b => b.id eqv id)
          .get
          .slotData
          .pure[F]
      )
      .anyNumberOfTimes()

    (blake2b256Resource
      .use[NonEmptyChain[RhoNonceHash]](_: Function1[Blake2b256, F[NonEmptyChain[RhoNonceHash]]]))
      .expects(*)
      .anyNumberOfTimes()
      .onCall { f: Function1[Blake2b256, F[NonEmptyChain[RhoNonceHash]]] => f(new Blake2b256) }

    (blake2b512Resource
      .use[NonEmptyChain[RhoNonceHash]](_: Function1[Blake2b512, F[NonEmptyChain[RhoNonceHash]]]))
      .expects(*)
      .anyNumberOfTimes()
      .onCall { f: Function1[Blake2b512, F[NonEmptyChain[RhoNonceHash]]] => f(new Blake2b512) }

    val actual =
      underTest
        .etaToBe(SlotId(blocks.last.slot, blocks.last.id), 16L)
        .unsafeRunSync()

    val expected =
      EtaCalculationSpec.expectedEta(
        Sized.strictUnsafe(bigBangHeader.eligibilityCertificate.eta),
        epoch + 1,
        blocks
          .map(_.eligibilityCertificate.vrfSig: ByteVector)
          .map(ed25519Vrf.proofToHash)
          .map(bytes => Rho(Sized.strictUnsafe(bytes: ByteString)))
      )

    actual shouldBe expected
  }

  it should "compute the eta for an epoch with only a genesis block" in {
    val clock = mock[ClockAlgebra[F]]
    val blake2b256Resource = mock[UnsafeResource[F, Blake2b256]]
    val blake2b512Resource = mock[UnsafeResource[F, Blake2b512]]
    val fetchSlotData = mockFunction[BlockId, F[SlotData]]
    val bigBangHeader = arbitraryHeader.arbitrary.first.copy(slot = 0L, parentSlot = -1L)

    (() => clock.slotsPerEpoch)
      .expects()
      .anyNumberOfTimes()
      .returning(15L.pure[F])

    val underTest =
      EtaCalculation
        .make[F](
          fetchSlotData,
          clock,
          Sized.strictUnsafe(bigBangHeader.eligibilityCertificate.eta),
          blake2b256Resource,
          blake2b512Resource
        )
        .unsafeRunSync()
    val epoch = 0L

    fetchSlotData
      .expects(bigBangHeader.id)
      .once()
      .returning(bigBangHeader.slotData.pure[F])

    (blake2b256Resource
      .use[NonEmptyChain[RhoNonceHash]](_: Function1[Blake2b256, F[NonEmptyChain[RhoNonceHash]]]))
      .expects(*)
      .anyNumberOfTimes()
      .onCall { f: Function1[Blake2b256, F[NonEmptyChain[RhoNonceHash]]] => f(new Blake2b256) }

    (blake2b512Resource
      .use[NonEmptyChain[RhoNonceHash]](_: Function1[Blake2b512, F[NonEmptyChain[RhoNonceHash]]]))
      .expects(*)
      .anyNumberOfTimes()
      .onCall { f: Function1[Blake2b512, F[NonEmptyChain[RhoNonceHash]]] => f(new Blake2b512) }

    val actual =
      underTest
        .etaToBe(SlotId(bigBangHeader.slot, bigBangHeader.id), 16L)
        .unsafeRunSync()

    val expected =
      EtaCalculationSpec.expectedEta(
        Sized.strictUnsafe(bigBangHeader.eligibilityCertificate.eta),
        epoch + 1,
        List(Rho(Sized.strictUnsafe(ed25519Vrf.proofToHash(bigBangHeader.eligibilityCertificate.vrfSig))))
      )

    actual shouldBe expected
  }
}

object EtaCalculationSpec {

  private[consensus] def expectedEta(previousEta: Eta, epoch: Epoch, rhoValues: List[Rho]): Eta = {
    implicit val blake2b256: Blake2b256 = new Blake2b256
    implicit val blake2b512: Blake2b512 = new Blake2b512
    val messages: List[Bytes] =
      List(previousEta.data) ++ List(ByteString.copyFrom(BigInt(epoch).toByteArray)) ++ rhoValues
        .map(_.sizedBytes.data)
        .map(rhoToRhoNonceHash)
    Sized.strictUnsafe(blake2b256.hash(messages.foldLeft(ByteString.EMPTY)(_ concat _)))
  }
}
