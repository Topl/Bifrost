package co.topl.consensus.interpreters

import cats.data.NonEmptyChain
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.algebras.testInterpreters.NoOpLogger
import co.topl.algebras.{ClockAlgebra, UnsafeResource}
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.BlockHeaderV2Ops
import co.topl.crypto.generation.KeyInitializer
import co.topl.crypto.generation.KeyInitializer.Instances.vrfInitializer
import co.topl.crypto.hash.{Blake2b256, Blake2b512}
import co.topl.crypto.signing.Ed25519VRF
import co.topl.models.ModelGenerators._
import co.topl.models.Proofs.Knowledge
import co.topl.models._
import co.topl.typeclasses.implicits._
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class EtaCalculationSpec
    extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with MockFactory
    with EitherValues {

  behavior of "EtaCalculation"

  type F[A] = IO[A]

  implicit private val logger: NoOpLogger[F] = new NoOpLogger[F]

  implicit private val ed25519Vrf: Ed25519VRF =
    Ed25519VRF.precomputed()

  private val blake2b256 = new Blake2b256
  private val blake2b512 = new Blake2b512

  it should "compute the eta for an epoch" in {
    val clock = mock[ClockAlgebra[F]]
    val blake2b256Resource = mock[UnsafeResource[F, Blake2b256]]
    val blake2b512Resource = mock[UnsafeResource[F, Blake2b512]]
    val bigBangHeader = arbitraryHeader.arbitrary.first.copy(slot = 0L, parentSlot = -1L)

    (() => clock.slotsPerEpoch)
      .expects()
      .anyNumberOfTimes()
      .returning(15L.pure[F])

    val fetchSlotData = mockFunction[TypedIdentifier, F[SlotData]]

    val underTest =
      EtaCalculation.Eval
        .make[F](
          fetchSlotData,
          clock,
          bigBangHeader.eligibilityCertificate.eta,
          blake2b256Resource,
          blake2b512Resource
        )
        .unsafeRunSync()
    val epoch = 0L
    val skVrf = KeyInitializer[SecretKeys.VrfEd25519].random()
    val args: List[(Slot, Knowledge.VrfEd25519)] = List.tabulate(8) { offset =>
      val slot = offset.toLong + 1
      val signature =
        ed25519Vrf.sign(
          skVrf,
          LeaderElectionValidation
            .VrfArgument(bigBangHeader.eligibilityCertificate.eta, slot)
            .signableBytes
        )
      slot -> signature
    }

    val blocks: List[BlockHeaderV2] =
      bigBangHeader ::
      LazyList
        .unfold(List(bigBangHeader)) {
          case items if items.length == args.length + 1 => None
          case items =>
            val (slot, signature) = args(items.length - 1)
            val nextHeader = headerGen(
              slotGen = Gen.const[Long](slot),
              parentSlotGen = Gen.const(items.last.slot),
              eligibilityCertificateGen = eligibilityCertificateGen.map(c =>
                c.copy(vrfSig = signature, eta = bigBangHeader.eligibilityCertificate.eta)
              ),
              parentHeaderIdGen = Gen.const(items.last.id)
            ).first
            (nextHeader -> (items :+ nextHeader)).some
        }
        .toList

    fetchSlotData
      .expects(*)
      .onCall((id: TypedIdentifier) =>
        blocks.find(b => byteByteVectorTupleAsTypedBytes(b.id) eqv id).get.slotData.pure[F]
      )
      .anyNumberOfTimes()

    (blake2b256Resource
      .use[NonEmptyChain[RhoNonceHash]](_: Function1[Blake2b256, F[NonEmptyChain[RhoNonceHash]]]))
      .expects(*)
      .anyNumberOfTimes()
      .onCall { f: Function1[Blake2b256, F[NonEmptyChain[RhoNonceHash]]] => f(blake2b256) }

    (blake2b512Resource
      .use[NonEmptyChain[RhoNonceHash]](_: Function1[Blake2b512, F[NonEmptyChain[RhoNonceHash]]]))
      .expects(*)
      .anyNumberOfTimes()
      .onCall { f: Function1[Blake2b512, F[NonEmptyChain[RhoNonceHash]]] => f(blake2b512) }

    val actual =
      underTest.etaToBe(blocks.last.slotId, 16L).unsafeRunSync()

    val expected =
      EtaCalculationSpec.expectedEta(
        bigBangHeader.eligibilityCertificate.eta,
        epoch + 1,
        blocks.map(_.eligibilityCertificate.vrfSig).map(ed25519Vrf.proofToHash)
      )

    actual shouldBe expected
  }

  it should "compute the eta for an epoch with only a genesis block" in {
    val clock = mock[ClockAlgebra[F]]
    val blake2b256Resource = mock[UnsafeResource[F, Blake2b256]]
    val blake2b512Resource = mock[UnsafeResource[F, Blake2b512]]
    val fetchSlotData = mockFunction[TypedIdentifier, F[SlotData]]
    val bigBangHeader = arbitraryHeader.arbitrary.first.copy(slot = 0L, parentSlot = -1L)

    (() => clock.slotsPerEpoch)
      .expects()
      .anyNumberOfTimes()
      .returning(15L.pure[F])

    val underTest =
      EtaCalculation.Eval
        .make[F](
          fetchSlotData,
          clock,
          bigBangHeader.eligibilityCertificate.eta,
          blake2b256Resource,
          blake2b512Resource
        )
        .unsafeRunSync()
    val epoch = 0L

    fetchSlotData
      .expects(byteByteVectorTupleAsTypedBytes(bigBangHeader.id))
      .once()
      .returning(bigBangHeader.slotData.pure[F])

    (blake2b256Resource
      .use[NonEmptyChain[RhoNonceHash]](_: Function1[Blake2b256, F[NonEmptyChain[RhoNonceHash]]]))
      .expects(*)
      .anyNumberOfTimes()
      .onCall { f: Function1[Blake2b256, F[NonEmptyChain[RhoNonceHash]]] => f(blake2b256) }

    (blake2b512Resource
      .use[NonEmptyChain[RhoNonceHash]](_: Function1[Blake2b512, F[NonEmptyChain[RhoNonceHash]]]))
      .expects(*)
      .anyNumberOfTimes()
      .onCall { f: Function1[Blake2b512, F[NonEmptyChain[RhoNonceHash]]] => f(blake2b512) }

    val actual =
      underTest.etaToBe(bigBangHeader.slotId, 16L).unsafeRunSync()

    val expected =
      EtaCalculationSpec.expectedEta(
        bigBangHeader.eligibilityCertificate.eta,
        epoch + 1,
        List(ed25519Vrf.proofToHash(bigBangHeader.eligibilityCertificate.vrfSig))
      )

    actual shouldBe expected
  }
}

object EtaCalculationSpec {

  implicit private val blake2b256: Blake2b256 = new Blake2b256
  implicit private val blake2b512: Blake2b512 = new Blake2b512

  private[consensus] def expectedEta(previousEta: Eta, epoch: Epoch, rhoValues: List[Rho]): Eta = {
    val messages: List[Bytes] =
      List(previousEta.data) ++ List(Bytes(BigInt(epoch).toByteArray)) ++ rhoValues
        .map(Ed25519VRF.rhoToRhoNonceHash)
        .map(_.sizedBytes.data)
    blake2b256.hash(Bytes.concat(messages))
  }
}
