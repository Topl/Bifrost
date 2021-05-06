package co.topl.crypto.signatures

import co.topl.crypto.signatures.eddsa.Ed25519
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import co.topl.crypto.utils.Hex

class Ed25519Spec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  property("with Ed25519, signed message should be verifiable with appropriate public key") {
    forAll { (seed1: Array[Byte], seed2: Array[Byte], message1: Array[Byte], message2: Array[Byte]) =>
      whenever(!seed1.sameElements(seed2) && !message1.sameElements(message2)) {
        val keyPair = Ed25519.createKeyPair(seed1)
        val keyPair2 = Ed25519.createKeyPair(seed2)

        val sig = Ed25519.sign(keyPair._1, message1)

        Ed25519.verify(sig, message1, keyPair._2) shouldBe true
        Ed25519.verify(sig, message1, keyPair2._2) should not be true
        Ed25519.verify(sig, message2, keyPair._2) should not be true

      }
    }
  }

}
