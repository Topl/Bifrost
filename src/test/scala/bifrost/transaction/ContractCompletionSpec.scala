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

class ContractCompletionSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Generated ContractCompletion Tx should be valid") {
    forAll(validContractCompletionGen) {
      contractCompletion: ContractCompletion =>
        val semanticValid = BifrostState.semanticValidity(contractCompletion)
        if (semanticValid.isFailure) {
          semanticValid.failed.get.printStackTrace()
        }
        semanticValid.isSuccess shouldBe true
    }
  }

  property("Transaction with modified signature should be invalid") {
    forAll(validContractCompletionGen) {
      contractCompletion: ContractCompletion =>
        val wrongSig: Array[Byte] =
          (contractCompletion.signatures.head._2.bytes.head + 1).toByte +:
          contractCompletion.signatures.head._2.bytes.tail

        val wrongSigs: Map[PublicKey25519Proposition, Signature25519] =
          contractCompletion.signatures +
            (contractCompletion.signatures.head._1 -> Signature25519(wrongSig))

        BifrostState.semanticValidity(contractCompletion.copy(signatures = wrongSigs)).isSuccess shouldBe false
    }
  }

}
