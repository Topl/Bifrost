package co.topl.crypto.generation

import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.generation.mnemonic.EntropySupport.arbitraryEntropy
import co.topl.crypto.signing.Curve25519
import co.topl.models.utility.Base58
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class Curve25519AxolotlKeyGenSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  property("with Curve25519, keyPairs generated with the same seed should be the same") {
    forAll { entropy: Entropy =>
      whenever(entropy.value.nonEmpty) {
        val curve25519 = new Curve25519

        val (sk1, vk1) = curve25519.createKeyPair(entropy, None)
        val (sk2, vk2) = curve25519.createKeyPair(entropy, None)

        sk1 === sk2 shouldBe true
        vk1 === vk2 shouldBe true
      }
    }
  }

  property("keys from a given entropy should deterministically generate a key pair") {
    val curve25519 = new Curve25519
    val pbkdf = new Pbkdf2Sha512()

    val entropy: Entropy =
      pbkdf.generateKey(Base58.decode("abc").getOrElse(Array.fill(32)(1: Byte)), "test".getBytes("utf-8"), 32, 4096)

    val (sk, vk) = curve25519.createKeyPair(entropy, None)
  }
}
