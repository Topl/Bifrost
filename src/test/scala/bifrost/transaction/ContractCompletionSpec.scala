package bifrost.transaction

/**
  * Created by cykoz on 5/11/2017.
  */
import bifrost.state.BifrostState
import bifrost.transaction.box.ReputationBox
import bifrost.transaction.box.proposition.MofNPropositionSerializer
import bifrost.{BifrostGenerators, ValidGenerators}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519

class ContractCompletionSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Generated ContractCompletion Tx should be valid") {
    forAll(validContractCompletionGen) {
      cc: ContractCompletion =>
        val semanticValid = BifrostState.semanticValidity(cc)
        if (semanticValid.isFailure) {
          semanticValid.failed.get.printStackTrace()
        }
        semanticValid.isSuccess shouldBe true
    }
  }

  property("Transaction with modified signature should be invalid") {
    forAll(validContractCompletionGen) {
      tx: ContractCompletion =>
        val wrongSig: Array[Byte] = (tx.signatures.head._2.bytes.head + 1).toByte +: tx.signatures.head._2.bytes.tail
        val wrongSigs: Map[PublicKey25519Proposition, Signature25519] = tx.signatures + (tx.signatures.head._1 -> Signature25519(wrongSig))
        BifrostState.semanticValidity(tx.copy(signatures = wrongSigs)).isSuccess shouldBe false
    }
  }

}
