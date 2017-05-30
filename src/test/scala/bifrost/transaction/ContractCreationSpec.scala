package bifrost.transaction

/**
  * Created by cykoz on 5/11/2017.
  */
import bifrost.{BifrostGenerators, ValidGenerators}
import bifrost.state.BifrostState
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.proof.Signature25519

class ContractCreationSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Generated ContractCreation Tx should be valid") {
    forAll(validContractCreationGen) {
      cc: ContractCreation =>
        val semanticValid = BifrostState.semanticValidity(cc)
        semanticValid.isSuccess shouldBe true
    }
  }

  property("Transaction with modified signature should be invalid") {
    forAll(contractCreationGen) {
      tx: ContractCreation =>
        val wrongSig: Array[Byte] = (tx.signatures.head.bytes.head + 1).toByte +: tx.signatures.head.bytes.tail
        val wrongSigs = (Signature25519(wrongSig) +: tx.signatures.tail).toIndexedSeq
        BifrostState.semanticValidity(tx.copy(signatures = wrongSigs)).isSuccess shouldBe false
    }
  }

  property("Transaction returns ContractBox and AssetBoxes for Hub") {
    // TODO
  }

}
