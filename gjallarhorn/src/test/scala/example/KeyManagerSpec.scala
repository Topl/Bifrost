package example

import example.KeyManager.getListOfFiles
import org.scalatest.{Matchers, WordSpec}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256

import scala.reflect.io.Path
import scala.util.Try

class KeyManagerSpec extends WordSpec with Matchers{
  val randomBytes1 = Blake2b256(java.util.UUID.randomUUID.toString)
  val randomBytes2 = Blake2b256(java.util.UUID.randomUUID.toString)
  val (sk1, pk1) = PrivateKey25519Companion.generateKeys(randomBytes1)
  val (sk2, pk2) = PrivateKey25519Companion.generateKeys(randomBytes2)
  val (sk3, pk3) =  PrivateKey25519Companion.generateKeys(Blake2b256("keyBytes"))
  val (sk4, pk4) =  PrivateKey25519Companion.generateKeys(randomBytes1)

  "A signed message" should {
    val messageBytes = Blake2b256("messageBytes")
    val messageToSign = Blake2b256(java.util.UUID.randomUUID.toString)

    "match its signature to the expected private key" in {
      //Entropic input to input for pub/priv keypair
      val proof = PrivateKey25519Companion.sign(sk1,messageToSign)
      assert(PrivateKey25519Companion.verify(messageToSign, pk1, proof))
    }
    "yield an invalid signature if signed with incorrect foreign key" in {
      val proof = PrivateKey25519Companion.sign(sk1,messageToSign)
      assert(!PrivateKey25519Companion.verify(messageToSign, pk2, proof))
    }
    "sign and verify as expected when msg is deterministic" in {
      val proof = PrivateKey25519Companion.sign(sk3,messageBytes)
      assert(PrivateKey25519Companion.verify(messageBytes, pk3, proof))
    }
  }

  "Generating keys" should {
    "come as a private key and public key pair" in {
      //Entropic input used to make pub/priv keypair
      assert(sk1 != null)
      assert(pk1 != null)
      assert(sk1.isInstanceOf[PrivateKey25519])
      assert(pk1.isInstanceOf[PublicKey25519Proposition])
    }

    "be deterministic" in {
      //Used the same entropic input
      assert(sk1.privKeyBytes === sk4.privKeyBytes)
      assert(pk1.equals(pk4))
    }

    "have sufficient randomness in entropic keypair gen" in {
      //Valid pretest to ensure sufficiently random input so no shared keys
      assert(randomBytes1 != randomBytes2)

      assert(sk1 != null && pk1 != null && sk1.isInstanceOf[PrivateKey25519] && pk1.isInstanceOf[PublicKey25519Proposition])
      assert(sk2 != null && pk2 != null && sk2.isInstanceOf[PrivateKey25519] && pk2.isInstanceOf[PublicKey25519Proposition])
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

  "A keyfile" should {
    val keyFileDir = "keyfiles/keyManagerTest"
    val path: Path = Path(keyFileDir)

    "export is formatted JSON to keystore file" in {
      Try(path.deleteRecursively())
      Try(path.createDirectory())
      val password = "password"
      val seed = Blake2b256(java.util.UUID.randomUUID.toString)
      val (_, _) = PrivateKey25519Companion.generateKeys(seed)
      val keyFile = KeyFile(password, seed, keyFileDir)
      val readFile = KeyFile.readFile((getListOfFiles(keyFileDir).head).getPath)
      assert(keyFile.equals(readFile))
      Try(path.deleteRecursively())
    }
    "have keys stored in the proper format" in {
      Try(path.deleteRecursively())
      Try(path.createDirectory())
      val password = "password"
      val seed = Blake2b256(java.util.UUID.randomUUID.toString)
      val (_, _) = PrivateKey25519Companion.generateKeys(seed)
      val keyFile = KeyFile(password, seed, keyFileDir)

      val privKey = keyFile.getPrivateKey(password).get
      assert(privKey.isInstanceOf[PrivateKey25519])
      val pubKey = privKey.publicKeyBytes
      assert(pubKey.isInstanceOf[Array[Byte]])
      Try(path.deleteRecursively())
    }
    "be used to import keys" in {
      Try(path.deleteRecursively())
      Try(path.createDirectory())
      val password = "password"
      val seed = Blake2b256(java.util.UUID.randomUUID.toString)
      val (priv, pub) = PrivateKey25519Companion.generateKeys(seed)
      val keyFile = KeyFile(password, seed, keyFileDir)

      val privKey = keyFile.getPrivateKey(password).get
      assert(privKey.privKeyBytes === priv.privKeyBytes)
      val pubKey = privKey.publicKeyBytes
      assert(pubKey === pub.pubKeyBytes)
      Try(path.deleteRecursively())
    }
  }
}
