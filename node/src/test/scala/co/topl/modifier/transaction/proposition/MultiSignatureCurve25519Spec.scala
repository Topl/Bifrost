package co.topl.transaction.proposition

import co.topl.attestation.keyManagement.PrivateKeyCurve25519
import co.topl.attestation.{
  PublicKeyPropositionCurve25519,
  ThresholdPropositionCurve25519,
  ThresholdSignatureCurve25519
}
import co.topl.utils.CoreGenerators
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import scala.collection.SortedSet

class MultiSignatureCurve25519Spec
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with CoreGenerators {

  property(
    "A MultiSignature25519 created from single Signature25519 " +
    "should be valid for oneOfNProposition"
  ) {

    forAll(keyPairSetGen) { s: Set[(PrivateKeyCurve25519, PublicKeyPropositionCurve25519)] =>
      val message = nonEmptyBytesGen.sample.get
      val signatures = s.map(_._1.sign(message))
      val pubKeyProps = SortedSet[PublicKeyPropositionCurve25519]() ++ s.map(_._2)
      val oneOfNProposition = ThresholdPropositionCurve25519(1, pubKeyProps)

      signatures
        .map(s => ThresholdSignatureCurve25519(Set(s)))
        .forall(_.isValid(oneOfNProposition, message)) shouldBe true
    }
  }

  property(
    "A MultiSignature25519 created from single Signature25519 " +
    "should not be valid for twoOfNProposition"
  ) {

    forAll(keyPairSetGen) { s: Set[(PrivateKeyCurve25519, PublicKeyPropositionCurve25519)] =>
      val message = nonEmptyBytesGen.sample.get
      val signatures = s.map(_._1.sign(message))
      val pubKeyProps = SortedSet[PublicKeyPropositionCurve25519]() ++ s.map(_._2)
      val oneOfNProposition = ThresholdPropositionCurve25519(2, pubKeyProps)

      signatures
        .map(s => ThresholdSignatureCurve25519(Set(s)))
        .forall(_.isValid(oneOfNProposition, message)) shouldBe false
    }
  }
}
