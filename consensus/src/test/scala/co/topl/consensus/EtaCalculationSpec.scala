package co.topl.consensus

import cats.implicits._
import co.topl.algebras.{BlockchainState, ClockAlgebra}
import co.topl.consensus.vrf.ProofToHash
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.signing.Ed25519VRF
import co.topl.models.ModelGenerators._
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized
import co.topl.typeclasses.implicits._
import co.topl.typeclasses.{BlockGenesis, KeyInitializer}
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

  type F[A] = Either[Throwable, A]

  it should "compute the eta for an epoch" in {
    val state = mock[BlockchainState[F]]
    val clock = mock[ClockAlgebra[F]]
    val underTest = EtaCalculation.Eval.make[F](state, clock)
    val genesis = BlockGenesis(Nil).value
    val epoch = 0L
    val previousEta = etaGen.first
    val skVrf = KeyInitializer[SecretKeys.VrfEd25519].random()
    val args = List.tabulate(8) { offset =>
      val slot = offset.toLong + 1
      val nonceSignature =
        Ed25519VRF.instance.sign(
          skVrf,
          LeaderElectionValidation
            .VrfArgument(previousEta, slot, LeaderElectionValidation.Tokens.Nonce)
            .signableBytes
        )
      slot -> nonceSignature
    }

    val blocks: List[BlockHeaderV2] =
      LazyList
        .unfold(List(genesis.headerV2)) {
          case items if items.length == args.length + 1 => None
          case items =>
            val (slot, nonceSignature) = args(items.length - 1)
            val nextHeader = headerGen(
              slotGen = Gen.const[Long](slot),
              eligibilityCertificateGen = eligibilityCertificateGen.map(c => c.copy(vrfNonceSig = nonceSignature)),
              parentHeaderIdGen = Gen.const(items.last.id)
            ).first
            (nextHeader -> (items :+ nextHeader)).some
        }
        .toList

    (state
      .lookupEta(_: Epoch))
      .expects(epoch - 1)
      .once()
      .returning(previousEta.some.pure[F])

    (() => state.genesis)
      .expects()
      .once()
      .returning(genesis.pure[F])

    (() => state.canonicalHead)
      .expects()
      .once()
      .returning(BlockV2(blocks.last, BlockBodyV2(blocks.last.id, Nil)).pure[F])

    (() => clock.slotsPerEpoch)
      .expects()
      .once()
      .returning(15L.pure[F])

    (state
      .lookupBlockHeader(_: TypedIdentifier))
      .expects(*)
      .onCall((id: TypedIdentifier) => blocks.find(_.id == id).pure[F])
      .repeated(blocks.length - 1)

    val actual =
      underTest.calculate(epoch).value

    val expected =
      EtaCalculationSpec.expectedEta(
        previousEta,
        epoch,
        args.map(_._2).map(ProofToHash.digest)
      )

    actual shouldBe expected
  }

  it should "compute the eta for an epoch with only a genesis block" in {
    val state = mock[BlockchainState[F]]
    val clock = mock[ClockAlgebra[F]]
    val underTest = EtaCalculation.Eval.make[F](state, clock)
    val genesis = BlockGenesis(Nil).value
    val epoch = 0L
    val previousEta = etaGen.first

    (state
      .lookupEta(_: Epoch))
      .expects(epoch - 1)
      .once()
      .returning(previousEta.some.pure[F])

    (() => state.genesis)
      .expects()
      .once()
      .returning(genesis.pure[F])

    (() => state.canonicalHead)
      .expects()
      .once()
      .returning(genesis.pure[F])

    (() => clock.slotsPerEpoch)
      .expects()
      .once()
      .returning(15L.pure[F])

    (state
      .lookupBlockHeader(_: TypedIdentifier))
      .expects(*)
      .never()

    val actual =
      underTest.calculate(epoch).value

    val expected =
      EtaCalculationSpec.expectedEta(
        previousEta,
        epoch,
        List(ProofToHash.digest(genesis.headerV2.eligibilityCertificate.vrfNonceSig))
      )

    actual shouldBe expected
  }

  // TODO: For this situation, destroy the node.  "Hard Fault"
  it should "compute the eta for an epoch with no blocks" in {
    val state = mock[BlockchainState[F]]
    val clock = mock[ClockAlgebra[F]]
    val underTest = EtaCalculation.Eval.make[F](state, clock)
    val genesis = BlockGenesis(Nil).value
    val epoch = 1L
    val previousEta = etaGen.first

    (state
      .lookupEta(_: Epoch))
      .expects(epoch - 1)
      .once()
      .returning(previousEta.some.pure[F])

    (() => state.genesis)
      .expects()
      .once()
      .returning(genesis.pure[F])

    (() => state.canonicalHead)
      .expects()
      .once()
      .returning(genesis.pure[F])

    (() => clock.slotsPerEpoch)
      .expects()
      .once()
      .returning(15L.pure[F])

    (state
      .lookupBlockHeader(_: TypedIdentifier))
      .expects(*)
      .never()

    val actual =
      underTest.calculate(epoch).value

    val expected =
      EtaCalculationSpec.expectedEta(
        previousEta,
        epoch,
        Nil
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
