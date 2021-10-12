package co.topl.consensus

import cats.Id
import cats.implicits._
import co.topl.consensus.LeaderElectionValidation.VrfConfig
import co.topl.consensus.algebras.{
  EtaValidationAlgebra,
  LeaderElectionValidationAlgebra,
  RegistrationLookupAlgebra,
  VrfRelativeStakeValidationLookupAlgebra
}
import co.topl.consensus.vrf.ProofToHash
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.signatures.Ed25519VRF
import co.topl.crypto.typeclasses.KeyInitializer
import co.topl.crypto.typeclasses.implicits._
import co.topl.models.ModelGenerators._
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Lengths, Ratio, Sized}
import co.topl.typeclasses.implicits._
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class BlockHeaderValidationSpec
    extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with MockFactory
    with EitherValues {

  behavior of "ConsensusValidation"

  type EvalF[A] = Id[A]

  private val leaderElectionInterpreter =
    LeaderElectionValidation.Eval.make[EvalF](
      VrfConfig(lddCutoff = 0, precision = 16, baselineDifficulty = Ratio(1, 15), amplitude = Ratio(2, 5))
    )

  implicit private val ed25519Vrf: Ed25519VRF =
    Ed25519VRF.precomputed()

  it should "invalidate blocks with non-forward slot" in {
    forAll(
      headerGen(slotGen = Gen.chooseNum[Slot](50L, 100L)),
      headerGen(slotGen = Gen.chooseNum[Slot](0, 49))
    ) { case (parent, child) =>
      whenever(child.slot <= parent.slot) {
        val nonceInterpreter = mock[EtaValidationAlgebra[EvalF]]
        val relativeStakeInterpreter = mock[VrfRelativeStakeValidationLookupAlgebra[EvalF]]
        val registrationInterpreter = mock[RegistrationLookupAlgebra[EvalF]]
        val underTest =
          BlockHeaderValidation.Eval.Stateful
            .make[EvalF](nonceInterpreter, relativeStakeInterpreter, leaderElectionInterpreter, registrationInterpreter)

        underTest.validate(child, parent).left.value shouldBe BlockHeaderValidationFailures
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
        val nonceInterpreter = mock[EtaValidationAlgebra[EvalF]]
        val relativeStakeInterpreter = mock[VrfRelativeStakeValidationLookupAlgebra[EvalF]]
        val registrationInterpreter = mock[RegistrationLookupAlgebra[EvalF]]
        val underTest =
          BlockHeaderValidation.Eval.Stateful
            .make[EvalF](nonceInterpreter, relativeStakeInterpreter, leaderElectionInterpreter, registrationInterpreter)

        underTest.validate(child, parent).left.value shouldBe BlockHeaderValidationFailures
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
        val etaInterpreter = mock[EtaValidationAlgebra[EvalF]]
        val relativeStakeInterpreter = mock[VrfRelativeStakeValidationLookupAlgebra[EvalF]]
        val registrationInterpreter = mock[RegistrationLookupAlgebra[EvalF]]
        val underTest =
          BlockHeaderValidation.Eval.Stateful
            .make[EvalF](etaInterpreter, relativeStakeInterpreter, leaderElectionInterpreter, registrationInterpreter)

        underTest.validate(child, parent).left.value shouldBe BlockHeaderValidationFailures
          .ParentMismatch(child.parentHeaderId, parent.id)
      }
    }
  }

  it should "invalidate blocks with syntactically incorrect VRF certificate for a particular nonce" in {
    forAll(
      headerGen(
        slotGen = Gen.chooseNum(0L, 50L),
        timestampGen = Gen.chooseNum(0L, 50L),
        heightGen = Gen.const(1L)
      ).flatMap(parent =>
        // The child block has a generated VRF Certificate (generated test/nonce proofs), meaning the proofs will not
        // match the epoch nonce `[1]` used in the test body
        headerGen(
          slotGen = Gen.chooseNum(51L, 100L),
          timestampGen = Gen.chooseNum(51L, 100L),
          heightGen = Gen.const(2L)
        )
          .map(parent -> _.copy(parentHeaderId = parent.id))
      ),
      etaGen
    ) { case ((parent, child), eta) =>
      val etaInterpreter = mock[EtaValidationAlgebra[EvalF]]
      val relativeStakeInterpreter = mock[VrfRelativeStakeValidationLookupAlgebra[EvalF]]
      val registrationInterpreter = mock[RegistrationLookupAlgebra[EvalF]]
      val underTest =
        BlockHeaderValidation.Eval.Stateful
          .make[EvalF](etaInterpreter, relativeStakeInterpreter, leaderElectionInterpreter, registrationInterpreter)

      (etaInterpreter
        .etaOf(_: SlotId))
        .expects((child.slot, child.id))
        .anyNumberOfTimes()
        // This epoch nonce does not satisfy the generated VRF certificate
        .returning(eta.pure[EvalF])

      underTest
        .validate(child, parent)
        .left
        .value shouldBe a[BlockHeaderValidationFailures.InvalidEligibilityCertificateEta]
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
      val etaInterpreter = mock[EtaValidationAlgebra[EvalF]]
      val relativeStakeInterpreter = mock[VrfRelativeStakeValidationLookupAlgebra[EvalF]]
      val registrationInterpreter = mock[RegistrationLookupAlgebra[EvalF]]
      (registrationInterpreter
        .registrationOf(_: SlotId, _: TaktikosAddress))
        .expects(*, *)
        .once()
        .returning(BlockHeaderValidationSpec.validRegistration(vrfSecret.verificationKey).some)

      val underTest =
        BlockHeaderValidation.Eval.Stateful
          .make[EvalF](etaInterpreter, relativeStakeInterpreter, leaderElectionInterpreter, registrationInterpreter)

      val (eligibilityCert, slot) =
        validEligibilityCertificate(vrfSecret, leaderElectionInterpreter, eta, relativeStake, parent.slot)

      val unsigned =
        BlockHeaderV2.Unsigned(
          parentHeaderId = parent.id,
          parentSlot = parent.slot,
          txRoot = txRoot,
          bloomFilter = bloomFilter,
          timestamp = System.currentTimeMillis(),
          height = parent.height + 1,
          slot = slot,
          eligibilityCertificate = eligibilityCert,
          metadata = None,
          address = address
        )

      val child =
        BlockHeaderV2(
          parentHeaderId = unsigned.parentHeaderId,
          parentSlot = unsigned.parentSlot,
          txRoot = unsigned.txRoot,
          bloomFilter = unsigned.bloomFilter,
          timestamp = unsigned.timestamp,
          height = unsigned.height,
          slot = unsigned.slot,
          eligibibilityCertificate = unsigned.eligibilityCertificate,
          operationalCertificate = validOperationalCertificate(unsigned),
          metadata = unsigned.metadata,
          address = unsigned.address
        )

      (etaInterpreter
        .etaOf(_: SlotId))
        .expects((child.slot, child.id))
        .anyNumberOfTimes()
        .returning(eta.pure[EvalF])

      (relativeStakeInterpreter
        .lookupAt(_: SlotId, _: TaktikosAddress))
        .expects((child.slot, child.id), *)
        .once()
        .returning(Ratio(0).some.pure[EvalF])

      underTest
        .validate(child, parent)
        .left
        .value shouldBe a[BlockHeaderValidationFailures.InvalidVrfThreshold]
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
      val etaInterpreter = mock[EtaValidationAlgebra[EvalF]]
      val relativeStakeInterpreter = mock[VrfRelativeStakeValidationLookupAlgebra[EvalF]]
      val registrationInterpreter = mock[RegistrationLookupAlgebra[EvalF]]
      (registrationInterpreter
        .registrationOf(_: SlotId, _: TaktikosAddress))
        .expects(*, *)
        .once()
        .returning(BlockHeaderValidationSpec.validRegistration(vrfSecret.verificationKey).some)

      val underTest =
        BlockHeaderValidation.Eval.Stateful
          .make[EvalF](etaInterpreter, relativeStakeInterpreter, leaderElectionInterpreter, registrationInterpreter)

      val (eligibilityCert, slot) =
        validEligibilityCertificate(vrfSecret, leaderElectionInterpreter, eta, relativeStake, parent.slot)

      val unsigned =
        BlockHeaderV2.Unsigned(
          parentHeaderId = parent.id,
          parentSlot = parent.slot,
          txRoot = txRoot,
          bloomFilter = bloomFilter,
          timestamp = System.currentTimeMillis(),
          height = parent.height + 1,
          slot = slot,
          eligibilityCertificate = eligibilityCert,
          metadata = None,
          address = address
        )

      val child =
        BlockHeaderV2(
          parentHeaderId = unsigned.parentHeaderId,
          parentSlot = unsigned.parentSlot,
          txRoot = unsigned.txRoot,
          bloomFilter = unsigned.bloomFilter,
          timestamp = unsigned.timestamp,
          height = unsigned.height,
          slot = unsigned.slot,
          eligibibilityCertificate = unsigned.eligibilityCertificate,
          operationalCertificate = validOperationalCertificate(unsigned),
          metadata = unsigned.metadata,
          address = unsigned.address
        )

      (etaInterpreter
        .etaOf(_: SlotId))
        .expects((child.slot, child.id))
        .anyNumberOfTimes()
        .returning(eta.pure[EvalF])

      (relativeStakeInterpreter
        .lookupAt(_: SlotId, _: TaktikosAddress))
        .expects((child.slot, child.id), *)
        .once()
        .returning(relativeStake.some.pure[EvalF])

      underTest.validate(child, parent).value shouldBe child
    }
  }

  private def validEligibilityCertificate(
    skVrf:                SecretKeys.Vrf,
    thresholdInterpreter: LeaderElectionValidationAlgebra[EvalF],
    eta:                  Eta,
    relativeStake:        Ratio,
    parentSlot:           Slot
  ): (EligibilityCertificate, Slot) = {
    def proof(slot: Slot, token: LeaderElectionValidation.Token) =
      Proofs.Signature.VrfEd25519(
        Sized.strictUnsafe(
          Bytes(
            ed25519Vrf.vrfProof(
              skVrf.ed25519.bytes.data.toArray,
              LeaderElectionValidation
                .VrfArgument(eta, slot, token)
                .signableBytes
                .toArray
            )
          )
        )
      )

    var slot = parentSlot + 1
    var testProof = proof(slot, LeaderElectionValidation.Tokens.Test)
    var threshold = thresholdInterpreter.getThreshold(relativeStake, slot)
    while (!thresholdInterpreter.isSlotLeaderForThreshold(threshold)(ProofToHash.digest(testProof))) {
      slot += 1
      testProof = proof(slot, LeaderElectionValidation.Tokens.Test)
      threshold = thresholdInterpreter.getThreshold(relativeStake, slot)
    }
    val cert = EligibilityCertificate(
      proof(slot, LeaderElectionValidation.Tokens.Nonce),
      testProof,
      skVrf.verificationKey[VerificationKeys.Vrf],
      threshold.evidence,
      eta
    )

    cert -> slot
  }

  private def validOperationalCertificate(unsigned: BlockHeaderV2.Unsigned): OperationalCertificate =
    OperationalCertificate(
      opSig = Proofs.Signature.HdKes(
        i = 0,
        vkI = VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0)))),
        ecSignature = Proofs.Signature.Ed25519(Sized.strictUnsafe(Bytes(Array.fill[Byte](64)(0)))),
        sigSumJ = Proofs.Signature.SumProduct(
          ecSignature = Proofs.Signature.Ed25519(Sized.strictUnsafe(Bytes(Array.fill[Byte](64)(0)))),
          vkK = VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0)))),
          index = 0,
          witness = Nil
        ),
        sigSumK = Proofs.Signature.SumProduct(
          ecSignature = Proofs.Signature.Ed25519(Sized.strictUnsafe(Bytes(Array.fill[Byte](64)(0)))),
          vkK = VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0)))),
          index = 0,
          witness = Nil
        )
      ),
      xvkM = VerificationKeys.ExtendedEd25519(
        VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0)))),
        Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0)))
      ),
      slotR = 0
    )

}

object BlockHeaderValidationSpec {

  def validRegistration(vkVrf: VerificationKeys.Vrf): Box.Values.TaktikosRegistration =
    Box.Values
      .TaktikosRegistration(
        Sized.strictUnsafe(
          Bytes(blake2b256.hash(vkVrf.ed25519.bytes.data.toArray).value)
        ),
        VerificationKeys.ExtendedEd25519(
          VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0)))),
          Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0)))
        ),
        0L
      )
}
