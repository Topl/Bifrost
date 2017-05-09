package bifrost.transaction

import bifrost.BifrostGenerators
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

class PolyTransferSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators {


  property("Attempting to validate a PolyTransfer without valid signature should error") {
    // Create invalid PolyTransfer
    // send tx to state
    forAll(polyTransferGen) { tx =>
      BifrostState.semanticValidity(tx).isSuccess shouldBe false
    }
  }

  property("Attempting to validate a PolyTransfer with valid signature should succeed") {
    forAll(validPolyTransferGen) { tx =>
      BifrostState.semanticValidity(tx).isSuccess shouldBe true
    }
  }

}
