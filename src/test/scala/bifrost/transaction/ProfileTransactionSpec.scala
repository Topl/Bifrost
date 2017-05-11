package bifrost.transaction

import bifrost.{BifrostGenerators, ValidGenerators}
import bifrost.state.BifrostState
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519

/**
  * Created by cykoz on 5/11/2017.
  */
class ProfileTransactionSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {
  property("Attempting to validate a ProfileTransaction without a key of 'role' should error") {
    forAll(profileTxGen) { tx =>
      BifrostState.semanticValidity(tx).isSuccess shouldBe false
    }
  }

  property("Attempting to validate a Profiletransaction with a key of 'role', but not a value of 'investor', 'producer', or 'hub' should error") {
    forAll(validProfileTransactionGen) {tx =>
      val wrongKeyValues = Map("role" -> "1")
      BifrostState.semanticValidity(tx.copy(keyValues = wrongKeyValues)).isSuccess shouldBe false
    }
  }

  property("Attempting to validate a Profiletransaction with modified from should error") {
    forAll(validProfileTransactionGen) { tx =>
      val wrongFrom: Array[Byte] = (tx.from.bytes.head + 1).toByte +: tx.from.bytes.tail
      BifrostState.semanticValidity(tx.copy(from = PublicKey25519Proposition(wrongFrom))).isSuccess shouldBe false
    }
  }

  property("Attempting to validate a Profiletransaction with modified signature should error") {
    forAll(validProfileTransactionGen) { tx =>
      val wrongSig: Array[Byte] = (tx.signature.bytes.head + 1).toByte +: tx.signature.bytes.tail
      BifrostState.semanticValidity(tx.copy(signature = Signature25519(wrongSig))).isSuccess shouldBe false
    }
  }
}
