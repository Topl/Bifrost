package bifrost.transaction

/**
  * Created by cykoz on 5/11/2017.
  */
import bifrost.state.BifrostState
import bifrost.{BifrostGenerators, ValidGenerators}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519

import scala.util.Success

class ContractCreationSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Generated ContractCreation Tx should be valid") {
    forAll(validContractCreationGen) {
      contractCreation: ContractCreation =>
        val semanticValid = BifrostState.semanticValidity(contractCreation)
        semanticValid shouldBe a[Success[Unit]]
    }
  }

  property("Tx with modified signature should be invalid") {
    forAll(validContractCreationGen) {
      contractCreation: ContractCreation =>
        val wrongSig: Array[Byte] =
          (contractCreation.signatures.head._2.bytes.head + 1).toByte +:
            contractCreation.signatures.head._2.bytes.tail

        val wrongSigs: Map[PublicKey25519Proposition, Signature25519] =
          contractCreation.signatures +
            (contractCreation.signatures.head._1 -> Signature25519(wrongSig))

        BifrostState.semanticValidity(contractCreation.copy(signatures = wrongSigs)).isSuccess shouldBe false
    }
  }
/*
  property("Tx with effective date in the past should be invalid") {

    lazy val pastEffDateAgreementGen: Gen[Agreement] = for {
      terms <- validAgreementTermsGen
      contractEndTime <- positiveLongGen
      assetCode <- stringGen
    } yield Agreement(terms, assetCode, Instant.now.toEpochMilli - 1L, contractEndTime)

    forAll(
      for {
        agreement <- pastEffDateAgreementGen
        parties <- partiesGen
        signature <- signatureGen
        fee <- positiveLongGen
        timestamp <- positiveLongGen
        numFeeBoxes <- positiveTinyIntGen
        numInvestmentBoxes <- positiveTinyIntGen
      } yield ContractCreation(
        agreement,
        (0 until numInvestmentBoxes).map { _ => positiveLongGen.sample.get -> positiveLongGen.sample.get },
        parties,
        parties.map { case (_, v) => (v, signatureGen.sample.get) },
        parties.map { case (_, v) => v -> (0 until numFeeBoxes).map { _ => preFeeBoxGen().sample.get } },
        parties.map { case (_, v) => v -> positiveTinyIntGen.sample.get.toLong },
        timestamp
      )
    ) {
      cc: ContractCreation =>
        val semanticValid = BifrostState.semanticValidity(cc)
        semanticValid.isSuccess shouldBe false
        semanticValid.failed.get shouldBe an[IllegalArgumentException]
    }
  }

  property("Tx with expiration date in the past should be invalid") {
    lazy val pastExpDateAgreementGen: Gen[Agreement] = for {
      terms <- validAgreementTermsGen
      contractEffectiveTime <- positiveLongGen
      assetCode <- stringGen
    } yield Agreement(terms, assetCode, contractEffectiveTime, Instant.now.toEpochMilli - 1L)

    forAll(
      for {
        agreement <- pastExpDateAgreementGen
        parties <- partiesGen
        signature <- signatureGen
        fee <- positiveLongGen
        timestamp <- positiveLongGen
        numFeeBoxes <- positiveTinyIntGen
        numInvestmentBoxes <- positiveTinyIntGen
      } yield ContractCreation(
        agreement,
        (0 until numInvestmentBoxes).map { _ => positiveLongGen.sample.get -> positiveLongGen.sample.get },
        parties,
        parties.map { case (_, v) => (v, signatureGen.sample.get) },
        parties.map { case (_, v) => v -> (0 until numFeeBoxes).map { _ => preFeeBoxGen().sample.get } },
        parties.map { case (_, v) => v -> positiveTinyIntGen.sample.get.toLong },
        timestamp
      )
    ) {
      cc: ContractCreation =>
        val semanticValid = BifrostState.semanticValidity(cc)
        semanticValid.isSuccess shouldBe false
        semanticValid.failed.get shouldBe an[IllegalArgumentException]
    }
  }

  property("Tx with valid expiration date before valid effective date should be invalid") {
    lazy val expBeforeEffAgreementGen: Gen[Agreement] = for {
      terms <- validAgreementTermsGen
      assetCode <- stringGen
    } yield Agreement(terms, assetCode, Instant.now.toEpochMilli + 10000L, Instant.now.toEpochMilli + 1000L)

    forAll(
      for {
        agreement <- expBeforeEffAgreementGen
        parties <- partiesGen
        signature <- signatureGen
        fee <- positiveLongGen
        timestamp <- positiveLongGen
        numFeeBoxes <- positiveTinyIntGen
        numInvestmentBoxes <- positiveTinyIntGen
      } yield ContractCreation(
        agreement,
        (0 until numInvestmentBoxes).map { _ => positiveLongGen.sample.get -> positiveLongGen.sample.get },
        parties,
        parties.map { case (_, v) => (v, signatureGen.sample.get) },
        parties.map { case (_, v) => v -> (0 until numFeeBoxes).map { _ => preFeeBoxGen().sample.get } },
        parties.map { case (_, v) => v -> positiveTinyIntGen.sample.get.toLong },
        timestamp
      )
    ) {
      cc: ContractCreation =>
        val semanticValid = BifrostState.semanticValidity(cc)
        semanticValid.isSuccess shouldBe false
        semanticValid.failed.get shouldBe an[IllegalArgumentException]
    }
  }*/

}
