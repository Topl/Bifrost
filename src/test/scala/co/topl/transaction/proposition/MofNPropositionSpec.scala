package co.topl.transaction.proposition

import co.topl.BifrostGenerators
import co.topl.crypto.PrivateKey25519
import co.topl.nodeView.state.box.proposition.MofNProposition
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import utils.NoShrink

class MofNPropositionSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with NoShrink {

  property("Any signature from set validates") {
    forAll(oneOfNPropositionGen) {
      case (keySet: Set[PrivateKey25519], mn: MofNProposition) =>
        val message = nonEmptyBytesGen.sample.getOrElse(Array.fill(positiveMediumIntGen.sample.get)(1: Byte))
        keySet
          .map(_.sign(message))
          .map(sig => mn.verify(message, sig.signature))
          .forall(next => next) shouldBe true
    }
  }
}