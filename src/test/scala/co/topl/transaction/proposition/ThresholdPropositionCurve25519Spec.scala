package co.topl.transaction.proposition

import co.topl.attestation.{PrivateKeyCurve25519, ThresholdPropositionCurve25519}
import co.topl.utils.{CoreGenerators, NoShrink}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class MofNPropositionSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with CoreGenerators
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