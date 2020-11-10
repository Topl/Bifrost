package co.topl.transaction.proposition

import co.topl.BifrostGenerators
import co.topl.attestation.proposition.ThresholdPropositionCurve25519
import co.topl.attestation.secrets.PrivateKeyCurve25519
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks }

class ThresholdPropositionCurve25519Spec extends AnyPropSpec
                                                 with ScalaCheckPropertyChecks
                                                 with ScalaCheckDrivenPropertyChecks
                                                 with Matchers
                                                 with BifrostGenerators {

  property("Any signature from set validates") {
    forAll(oneOfNPropositionGen) {
      case (keySet: Set[PrivateKeyCurve25519], mn: ThresholdPropositionCurve25519) =>
        val message = nonEmptyBytesGen.sample.get
        keySet
          .map(_.sign(message))
          .map(sig => mn.verify(message, sig.signature))
          .forall(next => next) shouldBe true
    }
  }
}