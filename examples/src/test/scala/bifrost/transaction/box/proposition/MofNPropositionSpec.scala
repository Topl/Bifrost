package bifrost.transaction.box.proposition

import bifrost.BifrostGenerators
import examples.bifrost.transaction.box.proposition.MofNProposition
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}

class MofNPropositionSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators {


  property("Any signature from set validates") {
    forAll(oneOfNPropositionGen) {
      case (keySet: Set[PrivateKey25519], mn: MofNProposition) =>
        val message = nonEmptyBytesGen.sample.get
        keySet.map(PrivateKey25519Companion.sign(_, message))
          .map(sig => mn.verify(message, sig.signature))
          .forall(next => next) shouldBe true
    }
  }
}