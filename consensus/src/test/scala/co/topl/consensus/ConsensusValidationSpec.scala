package co.topl.consensus

import cats.implicits._
import co.topl.consensus.KesCertifies.instances._
import co.topl.consensus.KesCertifies.ops._
import co.topl.crypto.typeclasses.Evolves.instances._
import co.topl.crypto.typeclasses.Evolves.ops._
import co.topl.crypto.typeclasses.KeyInitializer
import co.topl.crypto.typeclasses.KeyInitializer.Instances.kesInitializer
import co.topl.models.ModelGenerators._
import co.topl.models._
import co.topl.models.utility.{Lengths, Ratio}
import co.topl.typeclasses.ContainsEvidence.Instances._
import co.topl.typeclasses.ContainsEvidence.ops._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ConsensusValidationSpec
    extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with MockFactory
    with EitherValues {

  behavior of "ConsensusValidation"

  type EvalF[A] = Either[ConsensusValidation.Eval.Failure, A]

  private val leaderElectionInterpreter =
    LeaderElection.Eval.make[EvalF](
      Vrf.Config(lddCutoff = 0, precision = 16, baselineDifficulty = Ratio(1, 15), amplitude = Ratio(2, 5))
    )

  it should "invalidate blocks with non-forward slot" in {
    forAll(
      headerGen(slotGen = Gen.chooseNum[Slot](50L, 100L)),
      headerGen(slotGen = Gen.chooseNum[Slot](0, 49))
    ) { case (parent, child) =>
      whenever(child.slot <= parent.slot) {
        val nonceInterpreter = mock[EtaLookupAlgebra[EvalF]]
        val relativeStakeInterpreter = mock[VrfRelativeStakeLookupAlgebra[EvalF]]
        val underTest =
          ConsensusValidation.Eval.make[EvalF](nonceInterpreter, relativeStakeInterpreter, leaderElectionInterpreter)

        underTest.validate(child, parent).left.value shouldBe ConsensusValidation.Eval.Failures
          .NonForwardSlot(child.slot, parent.slot)
      }
    }
  }

  it should "invalidate blocks with non-forward timestamp" in {
    forAll(
      headerGen(timestampGen = Gen.chooseNum[Timestamp](51, 100), slotGen = Gen.chooseNum[Slot](0, 50)),
      headerGen(timestampGen = Gen.chooseNum[Timestamp](0, 50), slotGen = Gen.chooseNum[Slot](51, 100))
    ) { case (parent, child) =>
      whenever(child.slot > parent.slot && parent.timestamp >= child.timestamp) {
        val nonceInterpreter = mock[EtaLookupAlgebra[EvalF]]
        val relativeStakeInterpreter = mock[VrfRelativeStakeLookupAlgebra[EvalF]]
        val underTest =
          ConsensusValidation.Eval.make[EvalF](nonceInterpreter, relativeStakeInterpreter, leaderElectionInterpreter)

        underTest.validate(child, parent).left.value shouldBe ConsensusValidation.Eval.Failures
          .NonForwardTimestamp(child.timestamp, parent.timestamp)
      }
    }
  }

  it should "invalidate blocks with parent-header mismatch" in {
    forAll(
      headerGen(
        slotGen = Gen.chooseNum(0L, 50L),
        timestampGen = Gen.chooseNum(0L, 50L)
      ),
      headerGen(
        slotGen = Gen.chooseNum(51L, 100L),
        timestampGen = Gen.chooseNum(51L, 100L)
      )
    ) { case (parent, child) =>
      whenever(
        child.slot > parent.slot && child.timestamp > parent.timestamp && child.parentHeaderId != parent.id
      ) {
        val etaInterpreter = mock[EtaLookupAlgebra[EvalF]]
        val relativeStakeInterpreter = mock[VrfRelativeStakeLookupAlgebra[EvalF]]
        val underTest =
          ConsensusValidation.Eval.make[EvalF](etaInterpreter, relativeStakeInterpreter, leaderElectionInterpreter)

        underTest.validate(child, parent).left.value shouldBe ConsensusValidation.Eval.Failures
          .ParentMismatch(child.parentHeaderId, parent.id)
      }
    }
  }

  it should "invalidate blocks with syntactically incorrect VRF certificate for a particular nonce" in {
    forAll(
      headerGen(
        slotGen = Gen.chooseNum(0L, 50L),
        timestampGen = Gen.chooseNum(0L, 50L)
      ).flatMap(parent =>
        // The child block has a generated VRF Certificate (generated test/nonce proofs), meaning the proofs will not
        // match the epoch nonce `[1]` used in the test body
        headerGen(
          slotGen = Gen.chooseNum(51L, 100L),
          timestampGen = Gen.chooseNum(51L, 100L)
        )
          .map(parent -> _.copy(parentHeaderId = parent.id))
      ),
      etaGen
    ) { case ((parent, child), eta) =>
      val etaInterpreter = mock[EtaLookupAlgebra[EvalF]]
      val relativeStakeInterpreter = mock[VrfRelativeStakeLookupAlgebra[EvalF]]
      val underTest =
        ConsensusValidation.Eval.make[EvalF](etaInterpreter, relativeStakeInterpreter, leaderElectionInterpreter)

      (etaInterpreter
        .etaOf(_: BlockHeaderV2))
        .expects(child)
        .anyNumberOfTimes()
        // This epoch nonce does not satisfy the generated VRF certificate
        .returning(eta.pure[EvalF])

      underTest.validate(child, parent).left.value shouldBe ConsensusValidation.Eval.Failures
        .InvalidVrfCertificate(child.vrfCertificate)
    }
  }

  ignore should "invalidate blocks with a syntactically incorrect KES certificate" in {}

  ignore should "invalidate blocks with a semantically incorrect registration verification" in {}

  it should "invalidate blocks with an insufficient VRF threshold" in {
    forAll(
      headerGen(slotGen = Gen.const[Long](5000)),
      genSizedStrictBytes[Lengths.`32`.type]().flatMap(txRoot =>
        genSizedStrictBytes[Lengths.`256`.type]()
          .flatMap(bloomFilter => etaGen.map(nonce => (txRoot, bloomFilter, nonce)))
      ),
      relativeStakeGen,
      Gen.const(KeyInitializer.Instances.vrfInitializer.random()),
      taktikosAddressGen
    ) { case (parent, (txRoot, bloomFilter, eta), relativeStake, vrfSecret, address) =>
      val etaInterpreter = mock[EtaLookupAlgebra[EvalF]]
      val relativeStakeInterpreter = mock[VrfRelativeStakeLookupAlgebra[EvalF]]
      val underTest =
        ConsensusValidation.Eval.make[EvalF](etaInterpreter, relativeStakeInterpreter, leaderElectionInterpreter)

      val Some(hit) = LazyList
        .from(parent.slot.toInt + 1)
        .collectFirstSome(s =>
          leaderElectionInterpreter.getHit(vrfSecret, relativeStake, s, s - parent.slot, eta).value
        )
      val unsigned =
        BlockHeaderV2.Unsigned(
          parentHeaderId = parent.id,
          parentSlot = parent.slot,
          txRoot = txRoot,
          bloomFilter = bloomFilter,
          timestamp = System.currentTimeMillis(),
          height = parent.height + 1,
          slot = hit.slot,
          vrfCertificate = hit.cert,
          thresholdEvidence = hit.threshold.evidence,
          metadata = None,
          address = address
        )

      val kesKey = {
        implicit val slot: Slot = 5000
        KeyInitializer[PrivateKeys.Kes].random().evolveSteps(unsigned.slot - slot)
      }

      val child =
        BlockHeaderV2(
          parentHeaderId = unsigned.parentHeaderId,
          parentSlot = unsigned.parentSlot,
          txRoot = unsigned.txRoot,
          bloomFilter = unsigned.bloomFilter,
          timestamp = unsigned.timestamp,
          height = unsigned.height,
          slot = unsigned.slot,
          vrfCertificate = unsigned.vrfCertificate,
          kesCertificate = kesKey.certify(unsigned),
          thresholdEvidence = unsigned.thresholdEvidence,
          metadata = unsigned.metadata,
          address = unsigned.address
        )

      (etaInterpreter
        .etaOf(_: BlockHeaderV2))
        .expects(child)
        .anyNumberOfTimes()
        .returning(eta.pure[EvalF])

      (relativeStakeInterpreter
        .lookupAt(_: BlockHeaderV2)(_: TaktikosAddress))
        .expects(child, *)
        .once()
        .returning(Ratio(0).pure[EvalF])

      underTest
        .validate(child, parent)
        .left
        .value shouldBe a[ConsensusValidation.Eval.Failures.InvalidVrfThreshold]
    }
  }

  it should "validate valid blocks" in {
    forAll(
      headerGen(slotGen = Gen.const[Long](5000)),
      genSizedStrictBytes[Lengths.`32`.type]().flatMap(txRoot =>
        genSizedStrictBytes[Lengths.`256`.type]()
          .flatMap(bloomFilter => etaGen.map(nonce => (txRoot, bloomFilter, nonce)))
      ),
      relativeStakeGen,
      Gen.const(KeyInitializer.Instances.vrfInitializer.random()),
      taktikosAddressGen
    ) { case (parent, (txRoot, bloomFilter, eta), relativeStake, vrfSecret, address) =>
      val etaInterpreter = mock[EtaLookupAlgebra[EvalF]]
      val relativeStakeInterpreter = mock[VrfRelativeStakeLookupAlgebra[EvalF]]
      val underTest =
        ConsensusValidation.Eval.make[EvalF](etaInterpreter, relativeStakeInterpreter, leaderElectionInterpreter)

      val Some(hit) = LazyList
        .from(parent.slot.toInt + 1)
        .collectFirstSome(s =>
          leaderElectionInterpreter.getHit(vrfSecret, relativeStake, s, s - parent.slot, eta).value
        )
      val unsigned =
        BlockHeaderV2.Unsigned(
          parentHeaderId = parent.id,
          parentSlot = parent.slot,
          txRoot = txRoot,
          bloomFilter = bloomFilter,
          timestamp = System.currentTimeMillis(),
          height = parent.height + 1,
          slot = hit.slot,
          vrfCertificate = hit.cert,
          thresholdEvidence = hit.threshold.evidence,
          metadata = None,
          address = address
        )

      val kesKey = {
        implicit val slot: Slot = 5000
        KeyInitializer[PrivateKeys.Kes].random().evolveSteps(unsigned.slot - slot)
      }

      val child =
        BlockHeaderV2(
          parentHeaderId = unsigned.parentHeaderId,
          parentSlot = unsigned.parentSlot,
          txRoot = unsigned.txRoot,
          bloomFilter = unsigned.bloomFilter,
          timestamp = unsigned.timestamp,
          height = unsigned.height,
          slot = unsigned.slot,
          vrfCertificate = unsigned.vrfCertificate,
          kesCertificate = kesKey.certify(unsigned),
          thresholdEvidence = unsigned.thresholdEvidence,
          metadata = unsigned.metadata,
          address = unsigned.address
        )

      (etaInterpreter
        .etaOf(_: BlockHeaderV2))
        .expects(child)
        .anyNumberOfTimes()
        .returning(eta.pure[EvalF])

      (relativeStakeInterpreter
        .lookupAt(_: BlockHeaderV2)(_: TaktikosAddress))
        .expects(child, *)
        .once()
        .returning(relativeStake.pure[EvalF])

      underTest.validate(child, parent).value.header shouldBe child
    }
  }

}
