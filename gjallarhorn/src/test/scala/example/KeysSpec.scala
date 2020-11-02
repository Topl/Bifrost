package example

import crypto.{PrivateKey25519, PrivateKey25519Companion, PublicKey25519Proposition}
import keymanager.{KeyFile, Keys}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.util.encode.Base58
import scorex.crypto.hash.Blake2b256

import scala.reflect.io.Path
import scala.util.Try

class KeysSpec extends AsyncFlatSpec with Matchers {
  val randomBytes1 = Blake2b256(java.util.UUID.randomUUID.toString)
  val randomBytes2 = Blake2b256(java.util.UUID.randomUUID.toString)

  // Three keypairs for full test range, use library keygen method
  val (sk1, pk1) = PrivateKey25519Companion.generateKeys(randomBytes1)
  val (sk2, pk2) =  PrivateKey25519Companion.generateKeys(Blake2b256("sameEntropic")) //Hash of this string was taken as input instead of entropy
  val (sk3, pk3) =  PrivateKey25519Companion.generateKeys(randomBytes1)

  //Filepath to write keypairs
  val keyFileDir = "keyfiles/keyManagerTest"
  val path: Path = Path(keyFileDir)
  Try(path.deleteRecursively())
  Try(path.createDirectory())
  val password = "password"

  val seed1 = Blake2b256(java.util.UUID.randomUUID.toString)


  val keyManager = Keys(Set(), keyFileDir)
  var pubKeys: Set[PublicKey25519Proposition] = Set()
  var privKeys: Set[PrivateKey25519] = Set()
  var keyFiles: Set[KeyFile] = Set()

  //Hashmap where seeds map to public/private keypair
  val (priv, pub) = PrivateKey25519Companion.generateKeys(seed1)
  if (!keyManager.publicKeys.contains(pub)) {
    keyFiles += KeyFile(password, seed1, keyFileDir)
  }
  pubKeys += pub
  privKeys += priv

  //------------------------------------------------------------------------------------
  //Signed messages
  val messageBytes = Blake2b256("sameEntropic") //Should have same input to check determinism
  val messageToSign = Blake2b256(java.util.UUID.randomUUID.toString)

  it should "Match its signature to the expected sender private key" in {
      //Entropic input to input for pub/priv keypair
      val proof = PrivateKey25519Companion.sign(sk1,messageToSign)
      assert(PrivateKey25519Companion.verify(messageToSign, pk1, proof))
  }

  it should "Yield an invalid signature if signed with incorrect foreign key" in {
      val proof = PrivateKey25519Companion.sign(sk1,messageToSign)
      assert(!PrivateKey25519Companion.verify(messageToSign, pk2, proof))
  }

  it should "Sign and verify as expected when msg is deterministic" in {
      val proof = PrivateKey25519Companion.sign(sk2,messageBytes)
      //Utilize same input. Proving hashing function AND keygen methods are deterministic
      assert(PrivateKey25519Companion.verify(messageBytes, pk2, proof))
  }
  //------------------------------------------------------------------------------------
  //Key Generation

   it should "Come as a private key and public key pair" in {
      //Entropic input used to make pub/priv keypair
      assert(sk1 != null)
      assert(pk1 != null)
      assert(sk1.isInstanceOf[PrivateKey25519])
      assert(pk1.isInstanceOf[PublicKey25519Proposition])
   }
  it should "Be deterministic" in {
      //Used the same entropic input
      assert(sk1.privKeyBytes === sk3.privKeyBytes)
      assert(pk1.equals(pk3))
  }

  it should "Have sufficient randomness in entropic keypair gen" in {
    //Valid pretest to ensure sufficiently random input so no shared keys
    assert(randomBytes1 != randomBytes2)
    assert(sk1 != null && pk1 != null && sk1.isInstanceOf[PrivateKey25519] && pk1.isInstanceOf[PublicKey25519Proposition])
  }

  //------------------------------------------------------------------------------------
  //KeyManager

  it should "Have 1 keyfile" in {
    assert(keyManager.publicKeys.size == 1)
    assert(Keys.getListOfFiles(keyFileDir).size == 1)
  }

  it should "Be unlocked yes path" in {
    keyManager.unlockKeyFile(Base58.encode(pubKeys.head.pubKeyBytes), password)
    assert(keyManager.secrets.size == 1)
  }
  it should "Be locked yes path" in {
    keyManager.lockKeyFile(Base58.encode(pubKeys.head.pubKeyBytes), password)
    assert(keyManager.secrets.size == 0)
  }

  //------------------------------------------------------------------------------------
  //KeyFile

   //Export is formatted JSON to keystore file
  val exportedKeys: List[KeyFile] = Keys.getListOfFiles(keyFileDir)
    .map(file => KeyFile.readFile(file.getPath))

  keyFiles.foreach { key =>
    exportedKeys.contains(key) shouldBe true
  }

  it should "Have keys stored in the proper format" in {
    val privKey = keyFiles.head.getPrivateKey(password).get
    assert(privKey.isInstanceOf[PrivateKey25519])
    val pubKey = privKey.publicKeyBytes
    assert(pubKey.isInstanceOf[Array[Byte]])
  }
  it should "Be used to import keys" in {
    val privKey = keyFiles.head.getPrivateKey(password).get
    assert(privKey.privKeyBytes === privKeys.head.privKeyBytes)
    val pubKey = privKey.publicKeyBytes
    assert(pubKey === pubKeys.head.pubKeyBytes)
  }

  //------------------------------------------------------------------------------------

}
