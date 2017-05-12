package bifrost.transaction

/**
  * Created by cykoz on 5/11/2017.
  */
import bifrost.{BifrostGenerators, ValidGenerators}
import examples.bifrost.state.BifrostState
import examples.bifrost.transaction.{ContractCreation, PolyTransfer, PolyTransfer$}
import examples.hybrid.state.SimpleBoxTransaction
import hybrid.HybridGenerators
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.hash.Sha256

class ContractCreationSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Transaction with modified signature should be invalid") {
    forAll(contractCreationGen) {
      tx: ContractCreation =>
        val wrongSig: Array[Byte] = (tx.signatures.head.bytes.head + 1).toByte +: tx.signatures.head.bytes.tail
        val wrongSigs = (Signature25519(wrongSig) +: tx.signatures.tail).toIndexedSeq
        BifrostState.semanticValidity(tx.copy(signatures = wrongSigs)).isSuccess shouldBe false
    }
  }

}
