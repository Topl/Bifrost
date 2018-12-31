package bifrost.transaction

import bifrost.{BifrostGenerators, ValidGenerators}
import bifrost.state.BifrostState
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519

/**
  * Created by cykoz on 5/11/2017.
  */
class ProfileTransactionSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Generated ProfileTransaction should be valid") {
    forAll(validProfileTransactionGen) { profileTransaction =>
      BifrostState.semanticValidity(profileTransaction).isSuccess shouldBe true
    }
  }

  property("Attempting to validate a ProfileTransaction without a key of 'role' should error") {
    forAll(profileTxGen) { profileTransaction =>
      BifrostState.semanticValidity(profileTransaction).isSuccess shouldBe false
    }
  }

  property("Attempting to validate a Profiletransaction with a key of 'role', but not a value of 'investor', 'producer', or 'hub' should error") {
    forAll(validProfileTransactionGen) { profileTransaction =>
      val wrongKeyValues = Map("role" -> "1")
      BifrostState.semanticValidity(profileTransaction.copy(keyValues = wrongKeyValues)).isSuccess shouldBe false
    }
  }

  property("Attempting to validate a Profiletransaction with modified from should error") {
    forAll(validProfileTransactionGen) { profileTransaction =>
      val wrongFrom: Array[Byte] =
        (profileTransaction.from.bytes.head + 1).toByte +:
          profileTransaction.from.bytes.tail

      BifrostState
        .semanticValidity(profileTransaction.copy(from = PublicKey25519Proposition(wrongFrom)))
        .isSuccess shouldBe false
    }
  }

  property("Attempting to validate a Profiletransaction with modified signature should error") {
    forAll(validProfileTransactionGen) { profileTransaction =>
      val wrongSig: Array[Byte] =
        (profileTransaction.signature.bytes.head + 1).toByte +:
          profileTransaction.signature.bytes.tail

      BifrostState
        .semanticValidity(profileTransaction.copy(signature = Signature25519(wrongSig)))
        .isSuccess shouldBe false
    }
  }
}
