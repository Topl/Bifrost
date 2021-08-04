package co.topl.modifier.transaction.proposition

import co.topl.attestation.keyManagement.PrivateKeyCurve25519
import co.topl.attestation.{
  PublicKeyPropositionCurve25519,
  ThresholdPropositionCurve25519,
  ThresholdSignatureCurve25519
}
import co.topl.utils.CommonGenerators
import co.topl.utils.GeneratorOps.GeneratorOps
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.SortedSet

class MultiSignatureCurve25519Spec
    extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with CommonGenerators {

  property(
    "A MultiSignature25519 created from single Signature25519 " +
    "should be valid for oneOfNProposition"
  ) {

    forAll(keyPairSetCurve25519Gen) { s: Set[(PrivateKeyCurve25519, PublicKeyPropositionCurve25519)] =>
      val message = nonEmptyBytesGen.sampleFirst()
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

    forAll(keyPairSetCurve25519Gen) { s: Set[(PrivateKeyCurve25519, PublicKeyPropositionCurve25519)] =>
      val message = nonEmptyBytesGen.sampleFirst()
      val signatures = s.map(_._1.sign(message))
      val pubKeyProps = SortedSet[PublicKeyPropositionCurve25519]() ++ s.map(_._2)
      val oneOfNProposition = ThresholdPropositionCurve25519(2, pubKeyProps)

      signatures
        .map(s => ThresholdSignatureCurve25519(Set(s)))
        .forall(_.isValid(oneOfNProposition, message)) shouldBe false
    }
  }
}
