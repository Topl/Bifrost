package bifrost.serialization

import bifrost.blocks.{BifrostBlock, BifrostBlockCompanion}
import bifrost.contract.Agreement
import bifrost.transaction._
import bifrost.transaction.box.proposition.{MofNProposition, MofNPropositionSerializer}
import bifrost.transaction.box._
import bifrost.{BifrostGenerators, ValidGenerators}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.core.transaction.state.PrivateKey25519

/**
  * Created by cykoz on 4/12/17.
  */
class SerializationTests extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("oneOfNProposition Serialization") {
    forAll(oneOfNPropositionGen) {
      case (keySet: Set[PrivateKey25519], mn: MofNProposition) =>
        val parsed = MofNPropositionSerializer.parseBytes(MofNPropositionSerializer.toBytes(mn)).get
        val serialized = MofNPropositionSerializer.toBytes(parsed)
        serialized sameElements MofNPropositionSerializer.toBytes(mn)
    }
  }

  property("ContractBox Serialization") {
    forAll(contractBoxGen) {
      b: ContractBox =>
        val parsed = BifrostBoxSerializer.parseBytes(BifrostBoxSerializer.toBytes(b)).get
        val serialized = BifrostBoxSerializer.toBytes(parsed)
        serialized sameElements BifrostBoxSerializer.toBytes(b)
    }
  }

  property("PolyBox Serialization") {
    forAll(polyBoxGen) {
      b: PolyBox =>
        val parsed = BifrostBoxSerializer.parseBytes(BifrostBoxSerializer.toBytes(b)).get
        val serialized = BifrostBoxSerializer.toBytes(parsed)
        serialized shouldEqual BifrostBoxSerializer.toBytes(b)
    }
  }

  property("ArbitBox Serialization") {
    forAll(arbitBoxGen) {
      b: ArbitBox =>
        val parsed = BifrostBoxSerializer.parseBytes(BifrostBoxSerializer.toBytes(b)).get
        val serialized = BifrostBoxSerializer.toBytes(parsed)
        serialized shouldEqual BifrostBoxSerializer.toBytes(b)
    }
  }

  property("AssetBox Serialization") {
    forAll(assetBoxGen) {
      b: AssetBox =>
        val parsed = BifrostBoxSerializer.parseBytes(BifrostBoxSerializer.toBytes(b)).get
        val serialized = BifrostBoxSerializer.toBytes(parsed)
        serialized shouldEqual BifrostBoxSerializer.toBytes(b)
    }
  }

  property("Reputation Serialization") {
    forAll(reputationBoxGen) {
      b: ReputationBox =>
        val parsed = BifrostBoxSerializer.parseBytes(BifrostBoxSerializer.toBytes(b)).get
        val serialized = BifrostBoxSerializer.toBytes(parsed)
        serialized shouldEqual BifrostBoxSerializer.toBytes(b)
    }
  }

  property("ProfileBox Serialization") {
    forAll(profileBoxGen) {
      b: ProfileBox =>
        val parsed = BifrostBoxSerializer.parseBytes(BifrostBoxSerializer.toBytes(b)).get
        val serialized = BifrostBoxSerializer.toBytes(parsed)
        serialized shouldEqual BifrostBoxSerializer.toBytes(b)
    }
  }

  property("Agreement Serialization") {
    forAll(agreementGen) {
      a: Agreement =>
        val parsed = AgreementCompanion.parseBytes(AgreementCompanion.toBytes(a)).get
        AgreementCompanion.toBytes(parsed) shouldEqual AgreementCompanion.toBytes(a)
    }
  }

  property("PolyTransfer Serialization") {
    forAll(polyTransferGen) {
      sc: PolyTransfer =>
        val parsed = TransferTransactionCompanion.parseBytes(
          TransferTransactionCompanion.toBytes(sc)
        ).get
        TransferTransactionCompanion.toBytes(parsed) shouldEqual TransferTransactionCompanion.toBytes(sc)
    }
  }

  property("ArbitTransfer Serialization") {
    forAll(arbitTransferGen) {
      ac: ArbitTransfer =>
        val parsed = TransferTransactionCompanion.parseBytes(
          TransferTransactionCompanion.toBytes(ac)
        ).get
        TransferTransactionCompanion.toBytes(parsed) shouldEqual TransferTransactionCompanion.toBytes(ac)
    }
  }
  property("ContractCreation Serialization") {
    forAll(contractCreationGen) {
      c: ContractCreation =>
        val parsed = ContractTransactionCompanion.parseBytes(
          ContractTransactionCompanion.toBytes(c)
        ).get
        val parsedBytes = ContractTransactionCompanion.toBytes(parsed)
        val directParsedBytes = ContractTransactionCompanion.toBytes(c)
        parsedBytes.length shouldEqual directParsedBytes.length
        val res = parsedBytes.zip(directParsedBytes).map {
          case (p, directParsed) =>
            p shouldEqual directParsed
        }
    }
  }

  property("ContractMethodExecution Serialization") {
    forAll(contractMethodExecutionGen) {
      c: ContractMethodExecution =>
        val parsed = ContractTransactionCompanion.parseBytes(
          ContractTransactionCompanion.toBytes(c)
        ).get
        ContractTransactionCompanion.toBytes(parsed) sameElements ContractTransactionCompanion.toBytes(c)
    }
  }

  property("ContractCompletion Serialization") {
    forAll(contractCompletionGen) {
      c: ContractCompletion =>
        val parsed = ContractTransactionCompanion.parseBytes(
          ContractTransactionCompanion.toBytes(c)
        ).get
        ContractTransactionCompanion.toBytes(parsed) sameElements ContractTransactionCompanion.toBytes(c)
    }
  }

  property("ProfileTransaction Serialization") {
    forAll(profileTxGen) {
      p: ProfileTransaction =>
        val parsed = ProfileTransactionCompanion.parseBytes(
            ProfileTransactionCompanion.toBytes(p)
        ).get
        ProfileTransactionCompanion.toBytes(parsed) shouldEqual ProfileTransactionCompanion.toBytes(p)
    }
  }


  property("BifrostBlock Serialization") {
    forAll(bifrostBlockGen) {
      bb: BifrostBlock =>
        val parsed = BifrostBlockCompanion.parseBytes(BifrostBlockCompanion.toBytes(bb)).get
        BifrostBlockCompanion.toBytes(parsed) shouldEqual BifrostBlockCompanion.toBytes(bb)
    }
  }

  /* TODO Need a generator that generates erroneous JSON
  property("Agreement with no Nonce") {
    forAll(agreementGen) {
      a: Agreement =>
        val newTerms = parse("""{}""").getOrElse(Json.Null)
        a.terms.json = newTerms
        val parsed = AgreementCompanion.parseBytes(AgreementCompanion.toBytes(a)).get
        AgreementCompanion.toBytes(parsed) shouldEqual AgreementCompanion.toBytes(a)
    }
  }*/
}
