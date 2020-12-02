package co.topl.transaction.proposition

import co.topl.BifrostGenerators
import co.topl.attestation.{PrivateKeyCurve25519, ThresholdPropositionCurve25519}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import utils.NoShrink

class ThresholdPropositionCurve25519Spec extends AnyPropSpec
                                                 with ScalaCheckPropertyChecks
                                                 with ScalaCheckDrivenPropertyChecks
                                                 with Matchers
                                                 with BifrostGenerators
  with NoShrink {

  property("Any signature from set validates") {
    forAll(oneOfNPropositionGen) {
      case (keySet: Set[PrivateKeyCurve25519], mn: ThresholdPropositionCurve25519) =>
        val message = nonEmptyBytesGen.sample.getOrElse(Array.fill(positiveMediumIntGen.sample.get)(1: Byte))
        keySet
          .map(_.sign(message))
          .map(sig => mn.verify(message, sig.signature))
          .forall(next => next) shouldBe true
    }
  }
}