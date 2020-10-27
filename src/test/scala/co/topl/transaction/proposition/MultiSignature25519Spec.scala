package co.topl.transaction.proposition

import co.topl.CoreGenerators
import co.topl.crypto.{MultiSignature25519, PrivateKey25519}
import co.topl.nodeView.state.box.proposition.{MofNProposition, PublicKey25519Proposition}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class MultiSignature25519Spec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with CoreGenerators {


  property("A MultiSignature25519 created from single Signature25519 " +
             "should be valid for oneOfNProposition") {

    forAll(keyPairSetGen) {
      s: Set[(PrivateKey25519, PublicKey25519Proposition)] =>
        val message = nonEmptyBytesGen.sample.get
        val signatures = s.map(_._1.sign(message))
        val oneOfNProposition = MofNProposition(1, s.map(keyPair => keyPair._2.pubKeyBytes))

        require(signatures.map(s => MultiSignature25519(Set(s))).forall(ms => ms.isValid(oneOfNProposition, message)))
    }
  }

  property("A MultiSignature25519 created from single Signature25519 " +
             "should not be valid for twoOfNProposition") {

    forAll(keyPairSetGen) {
      s: Set[(PrivateKey25519, PublicKey25519Proposition)] =>
        val message = nonEmptyBytesGen.sample.get
        val signatures = s.map(_._1.sign(message))

        val oneOfNProposition = MofNProposition(2, s.map(keyPair => keyPair._2.pubKeyBytes))

        require(!signatures.map(s => MultiSignature25519(Set(s))).forall(ms => ms.isValid(oneOfNProposition, message)))
    }
  }
}