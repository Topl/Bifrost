package bifrost.crypto

import bifrost.keygen.KeyFile
import bifrost.crypto.hash.FastCryptographicHash
import bifrost.keygen.KeyFile.uuid
import scala.reflect.io.Path
import scala.util.Try
import org.scalatest._

/*
 * Test class for verifying BIP39 phrase translator class
 */

class Bip39Spec extends FlatSpec with Matchers {

  // sample uuid string
  val uuidString = uuid
  // language for phrase words
  val lang = "english.txt"
  //phrase translator
  val pt = new Bip39(lang)

  val preMD5 = "3d7914c7810cb343a5db65548cb5d66a"

  "The wordlists" should "pass MD5 checksum" in {
    assert(preMD5 == pt.phraseListMD5)
    pt.verify
  }

  "A seed phrase" should "be generated" in {
    val (seedHex,phrase) = pt.seedPhrase(uuidString)
    println("seed hex: "+seedHex)
    println("seed phrase: "+phrase)
  }

  "A seed phrase" should "be translated to hex" in{
    val phraseGood = "news excite upon nothing begin candy oblige situate figure method over tomato"
    val phraseBad = "this is a bad phrase that i hope will throw an error"
    val phraseShort = "news excite upon nothing begin candy oblige situate figure method over"
    val phraseMixed = "excite news upon nothing begin candy oblige situate figure method over tomato"
    val phraseColeman = "exercise crop sorry shiver jealous glue oblige evoke enrich cram air fringe"
    val hexColeman = "4f467f3de31778c7e60a704b064415ae"
    val (seedHex,phrase) = pt.seedPhrase(uuidString)

    Try(println(pt.phraseToHex(phraseGood)))
    Try(println(pt.phraseToHex(phraseBad)))
    Try(println(pt.phraseToHex(phraseMixed)))
    Try(println(pt.phraseToHex(phraseShort)))
    assert(pt.phraseToHex(phraseColeman) == hexColeman)
    assert(pt.phraseToHex(phrase) == seedHex)
  }

  "A key file" should "be generated" in {
    val keyFileDir = "/tmp/scorex/test-data/keyfiles/bip39test"
    val path: Path = Path(keyFileDir)
    Try(path.deleteRecursively())
    Try(path.createDirectory())
    val password = "password"
    val (seedHex,phrase) = pt.seedPhrase(uuidString)
    val seed1 = pt.hexToUuid(seedHex)
    val seed2 = pt.hexToUuid(pt.phraseToHex(phrase))
    val seed1Hash: Array[Byte] = FastCryptographicHash(seed1)
    val seed2Hash: Array[Byte] = FastCryptographicHash(seed2)
    val key1 = KeyFile(password, seed1Hash, keyFileDir)
    val key2 = KeyFile(password, seed2Hash, keyFileDir)
    val key3 = KeyFile(password = password, seed = FastCryptographicHash(uuidString),defaultKeyDir = keyFileDir)
    assert(key1.getPrivateKey(password).get.privKeyBytes.mkString("")
        == key2.getPrivateKey(password).get.privKeyBytes.mkString(""))
    assert(key2.getPrivateKey(password).get.privKeyBytes.mkString("")
        == key3.getPrivateKey(password).get.privKeyBytes.mkString(""))
    assert(key3.getPrivateKey(password).get.privKeyBytes.mkString("")
        == key1.getPrivateKey(password).get.privKeyBytes.mkString(""))
  }

}
