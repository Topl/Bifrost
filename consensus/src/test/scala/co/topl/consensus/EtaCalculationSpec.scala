package co.topl.consensus

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.signing.Ed25519VRF
import co.topl.models.ModelGenerators._
import co.topl.models.Proofs.Signature
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized
import co.topl.typeclasses._
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

  implicit private val ed25519Vrf: Ed25519VRF =
    Ed25519VRF.precomputed()

  it should "compute the eta for an epoch" in {
    val state = mock[SlotDataCache[F]]
    val clock = mock[ClockAlgebra[F]]
    val genesis = BlockGenesis(Nil).value
    val underTest =
      EtaCalculation.Eval.make[F](state, clock, genesis.headerV2.eligibilityCertificate.eta).unsafeRunSync()
    val epoch = 0L
    val skVrf = KeyInitializer[SecretKeys.VrfEd25519].random()
    val args: List[(Slot, Signature.VrfEd25519)] = List.tabulate(8) { offset =>
      val slot = offset.toLong + 1
      val nonceSignature =
        ed25519Vrf.sign(
          skVrf,
          LeaderElectionValidation
            .VrfArgument(genesis.headerV2.eligibilityCertificate.eta, slot, LeaderElectionValidation.Tokens.Nonce)
            .signableBytes
        )
      slot -> nonceSignature
    }

    val blocks: List[BlockHeaderV2] =
      genesis.headerV2 ::
      LazyList
        .unfold(List(genesis.headerV2)) {
          case items if items.length == args.length + 1 => None
          case items =>
            val (slot, nonceSignature) = args(items.length - 1)
            val nextHeader = headerGen(
              slotGen = Gen.const[Long](slot),
              parentSlotGen = Gen.const(items.last.slot),
              eligibilityCertificateGen = eligibilityCertificateGen.map(c =>
                c.copy(vrfNonceSig = nonceSignature, eta = genesis.headerV2.eligibilityCertificate.eta)
              ),
              parentHeaderIdGen = Gen.const(items.last.id)
            ).first
            (nextHeader -> (items :+ nextHeader)).some
        }
        .toList

    (() => clock.slotsPerEpoch)
      .expects()
      .anyNumberOfTimes()
      .returning(15L.pure[F])

    (state
      .get(_: TypedIdentifier))
      .expects(*)
      .onCall((id: TypedIdentifier) => SlotData(blocks.find(_.id eqv id).get).pure[F])
      .anyNumberOfTimes()

    val actual =
      underTest.etaToBe(blocks.last.slotId, 16L).unsafeRunSync()

    val expected =
      EtaCalculationSpec.expectedEta(
        genesis.headerV2.eligibilityCertificate.eta,
        epoch,
        blocks.map(_.eligibilityCertificate.vrfNonceSig).map(ed25519Vrf.proofToHash)
      )

    actual shouldBe expected
  }

  it should "compute the eta for an epoch with only a genesis block" in {
    val state = mock[SlotDataCache[F]]
    val clock = mock[ClockAlgebra[F]]
    val genesis = BlockGenesis(Nil).value
    val underTest =
      EtaCalculation.Eval.make[F](state, clock, genesis.headerV2.eligibilityCertificate.eta).unsafeRunSync()
    val epoch = 0L

    (() => clock.slotsPerEpoch)
      .expects()
      .anyNumberOfTimes()
      .returning(15L.pure[F])

    (state
      .get(_: TypedIdentifier))
      .expects(genesis.headerV2.id)
      .once()
      .returning(SlotData(genesis.headerV2).pure[F])

    val actual =
      underTest.etaToBe(genesis.headerV2.slotId, 16L).unsafeRunSync()

    val expected =
      EtaCalculationSpec.expectedEta(
        genesis.headerV2.eligibilityCertificate.eta,
        epoch,
        List(ed25519Vrf.proofToHash(genesis.headerV2.eligibilityCertificate.vrfNonceSig))
      )

    actual shouldBe expected
  }
}

object EtaCalculationSpec {

  private[consensus] def expectedEta(previousEta: Eta, epoch: Epoch, rhoValues: List[Rho]): Eta = {
    val messages: List[Bytes] =
      List(previousEta.data) ++ List(Bytes(BigInt(epoch).toByteArray)) ++ rhoValues.map(_.data)
    Sized.strictUnsafe(
      Bytes(
        blake2b256
          .hash(
            None,
            messages.map(_.toArray): _*
          )
          .value
      )
    )
  }
}
