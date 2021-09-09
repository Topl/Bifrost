package co.topl.consensus

import cats.Id
import cats.data.OptionT
import co.topl.algebras.ClockAlgebra
import co.topl.models.ModelGenerators._
import co.topl.models._
import co.topl.models.utility.{Lengths, Ratio}
import co.topl.typeclasses.ContainsEvidence.Instances._
import co.topl.typeclasses.ContainsEvidence.ops._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._
import co.topl.typeclasses.crypto.KeyInitializer
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ConsensusValidationProgramSpec
    extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with MockFactory
    with EitherValues {

  behavior of "ConsensusValidation"

  implicit val leaderElectionConfig: LeaderElection.Config =
    LeaderElection
      .Config(lddCutoff = 0, precision = 16, baselineDifficulty = Ratio(1, 15), amplitude = Ratio(2, 5))

  it should "invalidate blocks with non-forward slot" in {
    forAll(headerGen(slotGen = Gen.chooseNum(50L, 100L)), headerGen(slotGen = Gen.chooseNum[Long](20, 49))) {
      case (parent, child) =>
        whenever(child.slot <= parent.slot) {
          val nonceInterpreter = mock[EpochNoncesAlgebra[Id]]
          val relativeStakeInterpreter = mock[RelativeStateLookupAlgebra[Id]]
          val clockInterpreter = mock[ClockAlgebra[Id]]
          val underTest =
            new ConsensusValidationProgram[Id](nonceInterpreter, relativeStakeInterpreter, clockInterpreter)

          underTest.validate(child, parent).value.left.value shouldBe ConsensusValidationProgram.Failures
            .NonForwardSlot(child.slot, parent.slot)
        }
    }
  }

  it should "invalidate blocks with non-forward timestamp" in {
    forAll(headerGen(), headerGen()) { case (parent, child) =>
      whenever(child.slot > parent.slot && child.timestamp <= parent.timestamp) {
        val nonceInterpreter = mock[EpochNoncesAlgebra[Id]]
        val relativeStakeInterpreter = mock[RelativeStateLookupAlgebra[Id]]
        val clockInterpreter = mock[ClockAlgebra[Id]]
        val underTest = new ConsensusValidationProgram[Id](nonceInterpreter, relativeStakeInterpreter, clockInterpreter)

        underTest.validate(child, parent).value.left.value shouldBe ConsensusValidationProgram.Failures
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
      whenever(child.slot > parent.slot && child.timestamp > parent.timestamp && child.parentHeaderId != parent.id) {
        val nonceInterpreter = mock[EpochNoncesAlgebra[Id]]
        val relativeStakeInterpreter = mock[RelativeStateLookupAlgebra[Id]]
        val clockInterpreter = mock[ClockAlgebra[Id]]
        val underTest = new ConsensusValidationProgram[Id](nonceInterpreter, relativeStakeInterpreter, clockInterpreter)

        underTest.validate(child, parent).value.left.value shouldBe ConsensusValidationProgram.Failures
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
      )
    ) { case (parent, child) =>
      val nonceInterpreter = mock[EpochNoncesAlgebra[Id]]
      val relativeStakeInterpreter = mock[RelativeStateLookupAlgebra[Id]]
      val clockInterpreter = mock[ClockAlgebra[Id]]
      val underTest = new ConsensusValidationProgram[Id](nonceInterpreter, relativeStakeInterpreter, clockInterpreter)

      (() => clockInterpreter.slotsPerEpoch)
        .expects()
        .anyNumberOfTimes()
        .returning(1000)

      (nonceInterpreter
        .nonceForEpoch(_: Epoch))
        .expects(0L)
        .anyNumberOfTimes()
        // This epoch nonce does not satisfy the generated VRF certificate
        .returning(OptionT.pure[Id](Bytes(Array(1: Byte))))

      underTest.validate(child, parent).value.left.value shouldBe ConsensusValidationProgram.Failures
        .InvalidVrfCertificate(child.vrfCertificate)
    }
  }

  ignore should "invalidate blocks with a syntactically incorrect KES certificate" in {}

  ignore should "invalidate blocks with a semantically incorrect registration verification" in {}

  it should "invalidate blocks with an insufficient VRF threshold" in {
    forAll(
      headerGen(slotGen = Gen.const[Long](5000)),
      kesCertificateGen,
      genSizedStrictBytes[Lengths.`32`.type]().flatMap(txRoot =>
        genSizedStrictBytes[Lengths.`256`.type]()
          .flatMap(bloomFilter => epochNonceGen.map(nonce => (txRoot, bloomFilter, nonce)))
      ),
      relativeStakeGen,
      Gen.const(KeyInitializer.Instances.vrfInitializer.random()),
      taktikosAddressGen
    ) { case (parent, kesCertificate, (txRoot, bloomFilter, epochNonce), relativeStake, vrfSecret, address) =>
      val nonceInterpreter = mock[EpochNoncesAlgebra[Id]]
      val relativeStakeInterpreter = mock[RelativeStateLookupAlgebra[Id]]
      val clockInterpreter = mock[ClockAlgebra[Id]]
      val underTest = new ConsensusValidationProgram[Id](nonceInterpreter, relativeStakeInterpreter, clockInterpreter)

      val hit = LeaderElection.hits(vrfSecret, relativeStake, parent.slot + 1, parent.slot + 999, epochNonce).next()
      val child =
        BlockHeaderV2(
          parentHeaderId = parent.id,
          parentSlot = parent.slot,
          txRoot = txRoot,
          bloomFilter = bloomFilter,
          timestamp = System.currentTimeMillis(),
          height = parent.height + 1,
          slot = hit.slot,
          vrfCertificate = hit.cert,
          kesCertificate = kesCertificate,
          thresholdEvidence = hit.threshold.evidence,
          metadata = None,
          address = address
        )

      (() => clockInterpreter.slotsPerEpoch)
        .expects()
        .anyNumberOfTimes()
        .returning(1000)

      (nonceInterpreter
        .nonceForEpoch(_: Epoch))
        // The clock puts the child block in epoch 5, so validation should be concerned with epoch 5-2=3
        .expects(3L)
        .anyNumberOfTimes()
        .returning(OptionT.pure[Id](epochNonce))

      (relativeStakeInterpreter
        .lookup(_: Epoch)(_: TaktikosAddress))
        .expects(3L, *)
        .once()
        .returning(OptionT.pure[Id](Ratio(0)))

      underTest
        .validate(child, parent)
        .value
        .left
        .value shouldBe a[ConsensusValidationProgram.Failures.InvalidVrfThreshold]
    }
  }

  it should "validate valid blocks" in {
    forAll(
      headerGen(slotGen = Gen.const[Long](5000)),
      kesCertificateGen,
      genSizedStrictBytes[Lengths.`32`.type]().flatMap(txRoot =>
        genSizedStrictBytes[Lengths.`256`.type]()
          .flatMap(bloomFilter => epochNonceGen.map(nonce => (txRoot, bloomFilter, nonce)))
      ),
      relativeStakeGen,
      Gen.const(KeyInitializer.Instances.vrfInitializer.random()),
      taktikosAddressGen
    ) { case (parent, kesCertificate, (txRoot, bloomFilter, epochNonce), relativeStake, vrfSecret, address) =>
      val nonceInterpreter = mock[EpochNoncesAlgebra[Id]]
      val relativeStakeInterpreter = mock[RelativeStateLookupAlgebra[Id]]
      val clockInterpreter = mock[ClockAlgebra[Id]]
      val underTest = new ConsensusValidationProgram[Id](nonceInterpreter, relativeStakeInterpreter, clockInterpreter)

      val hit = LeaderElection.hits(vrfSecret, relativeStake, parent.slot + 1, parent.slot + 999, epochNonce).next()
      val child =
        BlockHeaderV2(
          parentHeaderId = parent.id,
          parentSlot = parent.slot,
          txRoot = txRoot,
          bloomFilter = bloomFilter,
          timestamp = System.currentTimeMillis(),
          height = parent.height + 1,
          slot = hit.slot,
          vrfCertificate = hit.cert,
          kesCertificate = kesCertificate,
          thresholdEvidence = hit.threshold.evidence,
          metadata = None,
          address = address
        )

      (() => clockInterpreter.slotsPerEpoch)
        .expects()
        .anyNumberOfTimes()
        .returning(1000)

      (nonceInterpreter
        .nonceForEpoch(_: Epoch))
        // The clock puts the child block in epoch 5, so validation should be concerned with epoch 5-2=3
        .expects(3L)
        .anyNumberOfTimes()
        .returning(OptionT.pure[Id](epochNonce))

      (relativeStakeInterpreter
        .lookup(_: Epoch)(_: TaktikosAddress))
        .expects(3L, *)
        .once()
        .returning(OptionT.pure[Id](relativeStake))

      underTest.validate(child, parent).value.value.header shouldBe child
    }
  }

}
