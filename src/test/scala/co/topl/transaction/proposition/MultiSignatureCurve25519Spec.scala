package co.topl.transaction.proposition

import co.topl.BifrostGenerators
import co.topl.attestation.{PrivateKeyCurve25519, PublicKeyPropositionCurve25519}
import co.topl.attestation.proposition.ThresholdPropositionCurve25519
import co.topl.attestation.proof.ThresholdSignatureCurve25519
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class MultiSignatureCurve25519Spec extends AnyPropSpec
                                   with ScalaCheckPropertyChecks
                                   with ScalaCheckDrivenPropertyChecks
                                   with Matchers
                                   with BifrostGenerators {


  property("A MultiSignature25519 created from single Signature25519 " +
             "should be valid for oneOfNProposition") {

    forAll(keyPairSetGen) {
      s: Set[(PrivateKeyCurve25519, PublicKeyPropositionCurve25519)] =>
        val message = nonEmptyBytesGen.sample.get
        val signatures = s.map(_._1.sign(message))
        val oneOfNProposition = ThresholdCurve25519Proposition(1, s.map(keyPair => keyPair._2.pubKeyBytes))

        require(signatures.map(s => ThresholdSignatureCurve25519(Set(s))).forall(ms => ms.isValid(oneOfNProposition, message)))
    }
  }

  property("A MultiSignature25519 created from single Signature25519 " +
             "should not be valid for twoOfNProposition") {

    forAll(keyPairSetGen) {
      s: Set[(PrivateKeyCurve25519, PublicKeyPropositionCurve25519)] =>
        val message = nonEmptyBytesGen.sample.get
        val signatures = s.map(_._1.sign(message))

        val oneOfNProposition = ThresholdCurve25519Proposition(2, s.map(keyPair => keyPair._2.pubKeyBytes))

        require(!signatures.map(s => ThresholdSignatureCurve25519(Set(s))).forall(ms => ms.isValid(oneOfNProposition, message)))
    }
  }
}