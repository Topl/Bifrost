package bifrost.transaction.proposition

import bifrost.BifrostGenerators
import bifrost.crypto.{PrivateKey25519, PrivateKey25519Companion}
import bifrost.modifier.box.proposition.MofNProposition
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class MofNPropositionSpec extends PropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with BifrostGenerators {


  property("Any signature from set validates") {
    forAll(oneOfNPropositionGen) {
      case (keySet: Set[PrivateKey25519], mn: MofNProposition) =>
        val message = nonEmptyBytesGen.sample.get
        keySet
          .map(PrivateKey25519Companion.sign(_, message))
          .map(sig => mn.verify(message, sig.signature))
          .forall(next => next) shouldBe true
    }
  }
}