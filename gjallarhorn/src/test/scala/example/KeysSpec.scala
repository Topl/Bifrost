package example

import java.io.File

import crypto.AddressEncoder.NetworkPrefix
import crypto.{Address, KeyfileCurve25519, PrivateKeyCurve25519, PublicKeyPropositionCurve25519}
import keymanager.{Keyfile, Keys}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.hash.{Blake2b256, Digest32}
import io.circe.syntax._
import org.scalatest.Ignore

import scala.reflect.io.Path
import scala.util.{Failure, Success, Try}

class KeysSpec extends AsyncFlatSpec with Matchers {

  /**
    * Make sure running bifrost in local network!
    */
  implicit val networkPrefix: NetworkPrefix = 48.toByte

  val keyFileDir = "keyfiles/keyManagerTest"
  val keyManager: Keys[PrivateKeyCurve25519, KeyfileCurve25519] = Keys(keyFileDir, KeyfileCurve25519)

  val randomBytes1: Digest32 = Blake2b256(java.util.UUID.randomUUID.toString)
  val randomBytes2: Digest32 = Blake2b256(java.util.UUID.randomUUID.toString)

  // Three keypairs for full test range, use library keygen method
  //val (sk1, pk1) = PrivateKeyCurve25519.generateKeys(randomBytes1)
  //val (sk2, pk2) =  PrivateKeyCurve25519.generateKeys(Blake2b256("sameEntropic")) //Hash of this string was taken as input instead of entropy
  //val (sk3, pk3) =  PrivateKeyCurve25519.generateKeys(randomBytes1)
  val privateKeys: Set[PrivateKeyCurve25519] = keyManager.generateNewKeyPairs(2, Some("keystest")) match {
    case Success(secrets) => secrets
    case Failure(ex) => throw new Error (s"Unable to generate new keys: $ex")
  }

  val sk3: PrivateKeyCurve25519 = keyManager.generateNewKeyPairs(1, Some("keystest")) match {
    case Success(secrets) => secrets.head
    case Failure(ex) => throw new Error (s"Unable to generate new keys: $ex")
  }

  val sk1: PrivateKeyCurve25519 = privateKeys.head
  val pk1: Address = sk1.publicImage.address
  val sk2: PrivateKeyCurve25519 = privateKeys.tail.head
  val pk2: Address = sk2.publicImage.address

  //Filepath to write keypairs
  val path: Path = Path(keyFileDir)
  Try(path.deleteRecursively())
  Try(path.createDirectory())
  val password = "password"

  val seedString: String = java.util.UUID.randomUUID.toString
  val seed1: Digest32 = Blake2b256(seedString)

  var pubKeys: Set[Address] = Set(pk1, pk2)
 /* var privKeys: Set[PrivateKeyCurve25519] = Set()
  var keyFiles: Set[KeyFile] = Set()

  //var (priv, pub) = PrivateKeyCurve25519.generateKeys(seed1)
  keyManager.generateKeyFile(password) match {
    case Success(value) => pub = value
    case Failure(e) => throw e
  }
  keyFiles += KeyFile.readFile(Keys.getListOfFiles(keyManager.defaultKeyDir).head.getPath)
  keyFiles.head.getPrivateKey(password) match {
    case Success(pk) => priv = pk
    case Failure(e) => throw e
  }
  pubKeys += pub
  privKeys += priv*/

  //------------------------------------------------------------------------------------
  //Signed messages
  val messageBytes: Digest32 = Blake2b256("sameEntropic") //Should have same input to check determinism
  val messageToSign: Digest32 = Blake2b256(java.util.UUID.randomUUID.toString)

  it should "Match its signature to the expected sender private key" in {
      //Entropic input to input for pub/priv keypair
      val proof = sk1.sign(messageToSign)
      assert(proof.isValid(sk1.publicImage, messageToSign))
  }

  it should "Yield an invalid signature if signed with incorrect foreign key" in {
      val proof = sk1.sign(messageToSign)
      assert(!proof.isValid(sk2.publicImage, messageToSign))
  }

  it should "Sign and verify as expected when msg is deterministic" in {
      val proof = sk2.sign(messageBytes)
      //Utilize same input. Proving hashing function AND keygen methods are deterministic
      assert(proof.isValid(sk2.publicImage, messageBytes))
  }
  //------------------------------------------------------------------------------------
  //Key Generation

  //Entropic input used to make pub/priv keypair
   it should "Come as a private key and public key pair" in {
     assert(sk1 != null)
     assert(pk1 != null)
     assert(sk1.isInstanceOf[PrivateKeyCurve25519])
     assert(sk1.publicImage.isInstanceOf[PublicKeyPropositionCurve25519])
     assert(pk1.isInstanceOf[Address])
   }
  it should "Be deterministic" in {
      //Used the same entropic input
      assert(pk1.equals(sk3.publicImage.address))
  }

  it should "Have sufficient randomness in entropic keypair gen" in {
    //Valid pretest to ensure sufficiently random input so no shared keys
    assert(randomBytes1 != randomBytes2)
    assert(sk1 != null && pk1 != null && sk1.isInstanceOf[PrivateKeyCurve25519] && pk1.isInstanceOf[Address])
  }

  //------------------------------------------------------------------------------------
  //KeyManager

  var pubKey: Address = sk1.publicImage.address
  val password2: String = "password2"

  it should "Have 2 keyfiles" in {
    assert(keyManager.addresses.size === 2)
    keyManager.generateKeyFile(password2) match {
      case Success(pk) =>
        pubKey = pk
        pubKeys += pubKey
      case Failure(ex) => throw new Error(s"An error occurred while creating a new keyfile. $ex")
    }
    assert(keyManager.addresses.count(addr => addr == pubKey) == 1)
    assert(keyManager.addresses.size === 3)
  }

  it should "Be locked" in {
    keyManager.lockKeyFile(pubKey.toString, password2).get
    assert(keyManager.addresses.size == 2)
  }

  it should "Be unlocked" in {
    keyManager.unlockKeyFile(pubKey.toString, password2)
    assert(keyManager.addresses.size == 3)
  }


  //------------------------------------------------------------------------------------
  //KeyFile

   //Export is formatted JSON to keystore file
/*  it should "grab list of files and convert to KeyFile" in {

    val exportedKeys: List[KeyFile] = Keys.getListOfFiles(keyManager.defaultKeyDir)
      .map(file => {
        KeyFile.readFile(file.getPath)
      })

    keyFiles.map(_.publicKeyFromAddress).foreach { pubKey =>
      assert(exportedKeys.map(_.publicKeyFromAddress).contains(pubKey))
    }
    assert (keyFiles.size == exportedKeys.size)
  }*/

/*  it should "Have keys stored in the proper format" in {
    val pswd: String =
      if (keyFiles.head.address == pubKey.address)
        password2
      else password

    val privKey = keyFiles.head.getPrivateKey(pswd).get
    assert(privKey.isInstanceOf[PrivateKeyCurve25519])
    val pKey = privKey.publicKeyBytes
    assert(pKey.isInstanceOf[Array[Byte]])
  }*/

  val seedPhrase = "stand earth guess employ goose aisle great next embark weapon wonder aisle monitor surface omit guilt model rule"
  it should "import keys correctly" in {
    keyManager.importPhrase("foo", seedPhrase, "en")
    assert(keyManager.addresses.size == 4)
  }

  //------------------------------------------------------------------------------------

}
