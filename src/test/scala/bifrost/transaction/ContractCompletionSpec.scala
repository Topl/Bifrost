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

  property("Contract completion correctly calculates and returns updated ReputationBox for Producer") {
    forAll(validContractCompletionGen) {
      cc: ContractCompletion =>
        val input: Long = cc.contract.agreement("terms").get.asObject.get("pledge").get.asNumber.get.toLong.get
        val deliveredAmount = cc.contract.storage("currentFulfillment").get.asObject.get("deliveredQuantity").get.asNumber.get.toLong.get

        val digest = FastCryptographicHash(MofNPropositionSerializer.toBytes(cc.proposition) ++ cc.hashNoNonces)
        val nonce = ContractTransaction.nonceFromDigest(digest)

        val (alphaSum: Double, betaSum: Double) = cc.producerReputation.foldLeft((0.0, 0.0))((a, b) => (a._1 + b.value._1, a._2 + b.value._2))
        val alpha: Double = alphaSum + (input.toDouble / 1000)*(2*deliveredAmount.toDouble/input.toDouble - 1)
        val beta: Double = betaSum + (input.toDouble / 1000)*(2 - deliveredAmount.toDouble/input.toDouble)

        cc.newBoxes.head shouldBe ReputationBox(cc.contract.Producer, nonce, (alpha, beta))
    }
  }

}
