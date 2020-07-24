package example

import org.scalatest.{Matchers, WordSpec}
import scorex.crypto.hash.Blake2b256

class KeyManagerSpec extends WordSpec with Matchers{
  "A signed message" should {
    "match its signature to the expected private key" in {
      val randomBytes = Blake2b256(java.util.UUID.randomUUID.toString)
      val (sk, pk) = PrivateKey25519Companion.generateKeys(randomBytes)

      val messageToSign = Blake2b256(java.util.UUID.randomUUID.toString)
      val proof = PrivateKey25519Companion.sign(sk,messageToSign)
      assert(PrivateKey25519Companion.verify(messageToSign, pk, proof))
    }
  }

  "Generating keys" should {
    "come as a private key and public key pair" in {
      val randomBytes = Blake2b256(java.util.UUID.randomUUID.toString)
      val (sk, pk) = PrivateKey25519Companion.generateKeys(randomBytes)
      assert(sk != null)
      assert(pk != null)
      assert(sk.isInstanceOf[PrivateKey25519])
      assert(pk.isInstanceOf[PublicKey25519Proposition])
    }
    "be deterministic" in {
      val randomBytes = Blake2b256(java.util.UUID.randomUUID.toString)
      val (sk1, pk1) = PrivateKey25519Companion.generateKeys(randomBytes)
      val (sk2, pk2) = PrivateKey25519Companion.generateKeys(randomBytes)
      assert(sk1.privKeyBytes === sk2.privKeyBytes)
      assert(pk1.equals(pk2))
    }
  }

  "A key file" should {
    "be locked" in {

    }
  }

}
