package example

import example.KeyManager.getListOfFiles
import org.scalatest.{Matchers, WordSpec}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256

import scala.reflect.io.Path
import scala.util.Try

class KeyManagerSpec extends WordSpec with Matchers {
  val randomBytes1 = Blake2b256(java.util.UUID.randomUUID.toString)
  val randomBytes2 = Blake2b256(java.util.UUID.randomUUID.toString)

  // Four keypairs for full test range
  val (sk1, pk1) = PrivateKey25519Companion.generateKeys(randomBytes1)
  val (sk2, pk2) = PrivateKey25519Companion.generateKeys(randomBytes2)
  val (sk3, pk3) =  PrivateKey25519Companion.generateKeys(Blake2b256("keyBytes")) //Hash of this string was taken as input instead of entropy
  val (sk4, pk4) =  PrivateKey25519Companion.generateKeys(randomBytes1)

  //Filepath to write keypairs
  val keyFileDir = "keyfiles/keyManagerTest"
  val path: Path = Path(keyFileDir)
  Try(path.deleteRecursively())
  Try(path.createDirectory())
  val password = "password"

  //Three seeds
  val seed1 = Blake2b256(java.util.UUID.randomUUID.toString)
  val seed2 = Blake2b256(java.util.UUID.randomUUID.toString)
  val seed3 = Blake2b256(java.util.UUID.randomUUID.toString)

  val seeds = Set(seed1, seed2, seed3)
  val keyManager = KeyManager(Set(), keyFileDir)
  var pubKeys: Set[PublicKey25519Proposition] = Set()
  var privKeys: Set[PrivateKey25519] = Set()
  var keyFiles: Set[KeyFile] = Set()

  //Hashmap where seeds map to public/private keypair
  seeds.map { seed =>
    val (priv, pub) = PrivateKey25519Companion.generateKeys(seed)
    if (!keyManager.publicKeys.contains(pub)) {
      keyFiles += KeyFile(password, seed, keyFileDir)
    }
    pubKeys += pub
    privKeys += priv
  }
  //------------------------------------------------------------------------------------
  //TEST ARCHETYPE: Signed messages
  "ARCHETYPE #1: A signed message" should {
    val messageBytes = Blake2b256("messageBytes")
    val messageToSign = Blake2b256(java.util.UUID.randomUUID.toString)

    "TEST #1: match its signature to the expected sender private key" in {
      //Entropic input to input for pub/priv keypair
      val proof = PrivateKey25519Companion.sign(sk1,messageToSign)
      assert(PrivateKey25519Companion.verify(messageToSign, pk1, proof))
    }
    "TEST #2: yield an invalid signature if signed with incorrect foreign key" in {
      val proof = PrivateKey25519Companion.sign(sk1,messageToSign)
      assert(!PrivateKey25519Companion.verify(messageToSign, pk2, proof))
    }
    "TEST #3: sign and verify as expected when msg is deterministic" in {
      val proof = PrivateKey25519Companion.sign(sk3,messageBytes)
      assert(PrivateKey25519Companion.verify(messageBytes, pk3, proof))
    }
  }
//------------------------------------------------------------------------------------
  //TEST ARCHETYPE: Key Generation
  "ARCHETYPE #2: Generating keys" should {
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
  //------------------------------------------------------------------------------------
  //TEST ARCHETYPE: KeyManager
  "ARCHETYPE #3: A key manager instance" should {
    "have 3 keyfiles" in {
      assert(keyManager.publicKeys.size == 3)
      assert(getListOfFiles(keyFileDir).size == 3)
    }
    "be unlocked yes path" in {
      keyManager.unlockKeyFile(Base58.encode(pubKeys.head.pubKeyBytes), password)
      assert(keyManager.secrets.size == 1)
    }
    "be locked yes path" in {
      keyManager.lockKeyFile(Base58.encode(pubKeys.head.pubKeyBytes), password)
      assert(keyManager.secrets.size == 0)
    }
  }
  //------------------------------------------------------------------------------------
  //TEST ARCHETYPE: KeyFile
  "ARCHETYPE #4: A keyfile" should {
    "export is formatted JSON to keystore file" in {
      val keyFile = keyFiles.head
      val readFile = KeyFile.readFile(getListOfFiles(keyFileDir)(0).getPath)
      assert(keyFile.equals(readFile))
    }
    "have keys stored in the proper format" in {
      val privKey = keyFiles.head.getPrivateKey(password).get
      assert(privKey.isInstanceOf[PrivateKey25519])
      val pubKey = privKey.publicKeyBytes
      assert(pubKey.isInstanceOf[Array[Byte]])
    }
    "be used to import keys" in {
      val privKey = keyFiles.head.getPrivateKey(password).get
      assert(privKey.privKeyBytes === privKeys.head.privKeyBytes)
      val pubKey = privKey.publicKeyBytes
      assert(pubKey === pubKeys.head.pubKeyBytes)
    }
  }
  //------------------------------------------------------------------------------------

}
