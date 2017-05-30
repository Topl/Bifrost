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
        semanticValid.isSuccess shouldBe true
    }
  }

  property("Transaction with modified signature should be invalid") {
    forAll(contractCompletionGen) {
      tx: ContractCompletion =>
        val wrongSig: Array[Byte] = (tx.signatures.head.bytes.head + 1).toByte +: tx.signatures.head.bytes.tail
        val wrongSigs = (Signature25519(wrongSig) +: tx.signatures.tail).toIndexedSeq
        BifrostState.semanticValidity(tx.copy(signatures = wrongSigs)).isSuccess shouldBe false
    }
  }

  property("Contract completion correctly calculates and returns updated ReputationBox for Producer") {
    forAll(validContractCompletionGen) {
      cc: ContractCompletion =>
        val input: Long = cc.contract.agreement("terms").get.asObject.get("pledge").get.asNumber.get.toLong.get
        val deliveredAmount = cc.contract.storage("currentFulfillment").get.asObject.get("deliveredQuantity").get.asNumber.get.toLong.get

        val digest = FastCryptographicHash(MofNPropositionSerializer.toBytes(cc.proposition) ++ cc.hashNoNonces)
        val nonce = ContractMethodExecution.nonceFromDigest(digest)

        val alpha: Double = (input.toDouble / 1000)*(2*deliveredAmount.toDouble/input.toDouble - 1)
        val beta: Double = (input.toDouble / 1000)*(2 - deliveredAmount.toDouble/input.toDouble )

        cc.newBoxes shouldBe Seq(ReputationBox(cc.contract.Producer, nonce, (alpha, beta)))
    }
  }

}
