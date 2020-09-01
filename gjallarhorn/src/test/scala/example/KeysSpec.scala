//FILE #3

package example

import crypto.{PrivateKey25519, PrivateKey25519Companion, PublicKey25519Proposition}
import keymanager.{KeyFile, Keys}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256

import scala.reflect.io.Path
import scala.util.Try

class KeysSpec extends AnyWordSpec with Matchers {
  val randomBytes1 = Blake2b256(java.util.UUID.randomUUID.toString)
  val randomBytes2 = Blake2b256(java.util.UUID.randomUUID.toString)

  // Four keypairs for full test range, use library keygen method
  val (sk1, pk1) = PrivateKey25519Companion.generateKeys(randomBytes1)
  val (sk2, pk2) = PrivateKey25519Companion.generateKeys(randomBytes2)
  val (sk3, pk3) =  PrivateKey25519Companion.generateKeys(Blake2b256("sameEntropic")) //Hash of this string was taken as input instead of entropy
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

  //Tripartite Seeds in JBOK configuration
  val seeds = Set(seed1, seed2, seed3)
  val keyManager = Keys(Set(), keyFileDir)
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
  //TEST ARCHETYPE: Signed Messages
  "[F3-A1-XX] ARCHETYPE: A signed message" should {
    val messageBytes = Blake2b256("sameEntropic") //Should have same input to check determinism
    val messageToSign = Blake2b256(java.util.UUID.randomUUID.toString)

    "[F3-A1-T1] TEST: Match its signature to the expected sender private key" in {
      //Entropic input to input for pub/priv keypair
      val proof = PrivateKey25519Companion.sign(sk1,messageToSign)
      assert(PrivateKey25519Companion.verify(messageToSign, pk1, proof))
    }
    "[F3-A1-T2] TEST: Yield an invalid signature if signed with incorrect foreign key" in {
      val proof = PrivateKey25519Companion.sign(sk1,messageToSign)
      assert(!PrivateKey25519Companion.verify(messageToSign, pk2, proof))
    }
    "[F3-A1-T3] TEST: Sign and verify as expected when msg is deterministic" in {
      val proof = PrivateKey25519Companion.sign(sk3,messageBytes)
      //Utilize same input. Proving hashing function AND keygen methods are deterministic
      assert(PrivateKey25519Companion.verify(messageBytes, pk3, proof))
    }
  }
  //------------------------------------------------------------------------------------
  //TEST ARCHETYPE: Key Generation
  "[F3-A2-XX] ARCHETYPE: Generating keys" should {
    "[F3-A2-T4] TEST: Come as a private key and public key pair" in {
      //Entropic input used to make pub/priv keypair
      assert(sk1 != null)
      assert(pk1 != null)
      assert(sk1.isInstanceOf[PrivateKey25519])
      assert(pk1.isInstanceOf[PublicKey25519Proposition])
    }

    "[F3-A2-T5] TEST: Be deterministic" in {
      //Used the same entropic input
      assert(sk1.privKeyBytes === sk4.privKeyBytes)
      assert(pk1.equals(pk4))
    }

    "[F3-A2-T6] TEST: Have sufficient randomness in entropic keypair gen" in {
      //Valid pretest to ensure sufficiently random input so no shared keys
      assert(randomBytes1 != randomBytes2)

      assert(sk1 != null && pk1 != null && sk1.isInstanceOf[PrivateKey25519] && pk1.isInstanceOf[PublicKey25519Proposition])
      assert(sk2 != null && pk2 != null && sk2.isInstanceOf[PrivateKey25519] && pk2.isInstanceOf[PublicKey25519Proposition])
    }

//    "[F3-A2-T7] TEST: Private Key is sufficiently long, 256-bit/32-byte length" in {
//      assert((pk1.asInstanceOf[String]).length() == 256 || ((sk1.privKeyBytes).asInstanceOf[String]).length() == 32)
//    }
  }
  //------------------------------------------------------------------------------------
  //TEST ARCHETYPE: KeyManager
  "[F3-A3-XX] ARCHETYPE: A key manager instance" should {
    "[F3-A3-T8] TEST: Have 3 keyfiles" in {
      assert(keyManager.publicKeys.size == 3)
      assert(Keys.getListOfFiles(keyFileDir).size == 3)
    }
    "[F3-A3-T9] TEST: Be unlocked yes path" in {
      keyManager.unlockKeyFile(Base58.encode(pubKeys.head.pubKeyBytes), password)
      assert(keyManager.secrets.size == 1)
    }
    "[F3-A3-T10] TEST: Be locked yes path" in {
      keyManager.lockKeyFile(Base58.encode(pubKeys.head.pubKeyBytes), password)
      assert(keyManager.secrets.size == 0)
    }

//    "[F3-A3-T11] TEST: Confirm JBOK Multiple Keyfile Relationship" in {
//      //Sufficient that second keypair is generated purely from entropy (randomBytes)
//      assert((sk2, pk2) == PrivateKey25519Companion.generateKeys(randomBytes2))
//    }
  }
  //------------------------------------------------------------------------------------
  //TEST ARCHETYPE: KeyFile
  "[F3-A4-XX] ARCHETYPE: A keyfile" should {
    "TEST: Export is formatted JSON to keystore file" in {
      val keyFile = keyFiles.head
      val readFile = KeyFile.readFile(Keys.getListOfFiles(keyFileDir)(0).getPath)
      assert(keyFile.equals(readFile))
    }
    "[F3-A4-12] TEST: Have keys stored in the proper format" in {
      val privKey = keyFiles.head.getPrivateKey(password).get
      assert(privKey.isInstanceOf[PrivateKey25519])
      val pubKey = privKey.publicKeyBytes
      assert(pubKey.isInstanceOf[Array[Byte]])
    }
    "[F3-A4-13] TEST: Be used to import keys" in {
      val privKey = keyFiles.head.getPrivateKey(password).get
      assert(privKey.privKeyBytes === privKeys.head.privKeyBytes)
      val pubKey = privKey.publicKeyBytes
      assert(pubKey === pubKeys.head.pubKeyBytes)
    }
  }
  //------------------------------------------------------------------------------------

}
/*
  TEST FORMAT (FAT)
    - F: File #
    - A: Archetype to group relevant tests
    - T: Specific Test
*/