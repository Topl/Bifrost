package example

import org.scalatest.{Matchers, WordSpec}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256

import scala.reflect.io.Path
import scala.util.Try

class KeyManagerSpec extends WordSpec with Matchers{
  "A signed message" should {
    "match its signature to the expected private key" in {
      //Entropic input to input for pub/priv keypair
      val randomBytes = Blake2b256(java.util.UUID.randomUUID.toString)
      val (sk, pk) = PrivateKey25519Companion.generateKeys(randomBytes)

      val messageToSign = Blake2b256(java.util.UUID.randomUUID.toString)
      val proof = PrivateKey25519Companion.sign(sk,messageToSign)
      assert(PrivateKey25519Companion.verify(messageToSign, pk, proof))
    }
    "yield an invalid signature if signed with incorrect foreign key" in {
      val randomBytes1 = Blake2b256(java.util.UUID.randomUUID.toString)
      val randomBytes2 = Blake2b256(java.util.UUID.randomUUID.toString)
      val (sk1, _) = PrivateKey25519Companion.generateKeys(randomBytes1)
      val (_, pk2) = PrivateKey25519Companion.generateKeys(randomBytes2)

      val messageToSign = Blake2b256(java.util.UUID.randomUUID.toString)
      val proof = PrivateKey25519Companion.sign(sk1,messageToSign)
      assert(!PrivateKey25519Companion.verify(messageToSign, pk2, proof))
    }
  }

  "Generating keys" should {
    "come as a private key and public key pair" in {
      //Entropic input used to make pub/priv keypair
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

      //Used the same entropic input
      assert(sk1.privKeyBytes === sk2.privKeyBytes)
      assert(pk1.equals(pk2))
    }

    "have sufficient randomness in entropic keypair gen" in {
      //Assumption: This method call is sufficiently random so no collision
      val randomBytesALICE = Blake2b256(java.util.UUID.randomUUID.toString)
      val randomBytesBOB = Blake2b256(java.util.UUID.randomUUID.toString)

      //Valid pretest to ensure sufficiently random input so no shared keys
      assert(randomBytesALICE != randomBytesBOB)

      val (skALICE, pkALICE) = PrivateKey25519Companion.generateKeys(randomBytesALICE)
      val (skBOB, pkBOB) = PrivateKey25519Companion.generateKeys(randomBytesBOB)

      assert(skALICE != null && pkALICE != null && skALICE.isInstanceOf[PrivateKey25519] && pkALICE.isInstanceOf[PublicKey25519Proposition])
      assert(skBOB != null && pkBOB != null && skBOB.isInstanceOf[PrivateKey25519] && pkBOB.isInstanceOf[PublicKey25519Proposition])
    }
  }

  "A key manager instance" should {

    val keyFileDir = "keyfiles/keyManagerTest"
    val path: Path = Path(keyFileDir)

    "have 3 keyfiles" in {
      Try(path.deleteRecursively())
      Try(path.createDirectory())
      val password = "password"

      val seed1 = Blake2b256(java.util.UUID.randomUUID.toString)
      val seed2 = Blake2b256(java.util.UUID.randomUUID.toString)
      val seed3 = Blake2b256(java.util.UUID.randomUUID.toString)

      val seeds = Set(seed1, seed2, seed3)
      val keyManager = KeyManager(Set(), keyFileDir)

      seeds.map { seed =>
        val (_, pub) = PrivateKey25519Companion.generateKeys(seed)
        if (!keyManager.publicKeys.contains(pub)) {
          KeyFile(password, seed, keyFileDir)
        }
        pub
      }
      assert(keyManager.publicKeys.size == 3)

      Try(path.deleteRecursively())
    }
    "be unlocked yes path" in {
      Try(path.deleteRecursively())
      Try(path.createDirectory())
      val password = "password"
      val seed = Blake2b256(java.util.UUID.randomUUID.toString)
      val (_, pub) = PrivateKey25519Companion.generateKeys(seed)
      KeyFile(password, seed, keyFileDir)

      val keyManager = KeyManager(Set(), keyFileDir)

      keyManager.unlockKeyFile(Base58.encode(pub.pubKeyBytes), password)
      assert(keyManager.secrets.size == 1)
      Try(path.deleteRecursively())
    }
    "be locked yes path" in {
      Try(path.deleteRecursively())
      Try(path.createDirectory())
      val password = "password"
      val seed = Blake2b256(java.util.UUID.randomUUID.toString)
      val (_, pub) = PrivateKey25519Companion.generateKeys(seed)
      KeyFile(password, seed, keyFileDir)

      val keyManager = KeyManager(Set(), keyFileDir)

      keyManager.unlockKeyFile(Base58.encode(pub.pubKeyBytes), password)
      keyManager.lockKeyFile(Base58.encode(pub.pubKeyBytes), password)
      assert(keyManager.secrets.size == 0)
      Try(path.deleteRecursively())
    }
  }
}
