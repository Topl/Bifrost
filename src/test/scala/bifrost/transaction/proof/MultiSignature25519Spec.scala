package bifrost.transaction.proof

import bifrost.BifrostGenerators
import bifrost.transaction.box.proposition.MofNProposition
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.state.{PrivateKey25519, PrivateKey25519Companion}

class MultiSignature25519Spec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators {


  property("A MultiSignature25519 created from single Signature25519 " +
             "should be valid for oneOfNProposition") {

    forAll(keyPairSetGen) {
      s: Set[(PrivateKey25519, PublicKey25519Proposition)] =>
        val message = nonEmptyBytesGen.sample.get
        val signatures = s.map(keyPair => PrivateKey25519Companion.sign(keyPair._1, message))
        val oneOfNProposition = MofNProposition(1, s.map(keyPair => keyPair._2.pubKeyBytes))

        require(signatures.map(s => MultiSignature25519(Set(s))).forall(ms => ms.isValid(oneOfNProposition, message)))
    }
  }

  property("A MultiSignature25519 created from single Signature25519 " +
             "should not be valid for twoOfNProposition") {

    forAll(keyPairSetGen) {
      s: Set[(PrivateKey25519, PublicKey25519Proposition)] =>
        val message = nonEmptyBytesGen.sample.get
        val signatures = s.map(keyPair => PrivateKey25519Companion.sign(keyPair._1, message))

        val oneOfNProposition = MofNProposition(2, s.map(keyPair => keyPair._2.pubKeyBytes))

        require(!signatures.map(s => MultiSignature25519(Set(s))).forall(ms => ms.isValid(oneOfNProposition, message)))
    }
  }
}