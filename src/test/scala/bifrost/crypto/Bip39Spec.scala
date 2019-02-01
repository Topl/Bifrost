package bifrost.crypto

import bifrost.keygen.KeyFile
import bifrost.crypto.hash.FastCryptographicHash
import scala.reflect.io.Path
import scala.util.Try
import org.scalatest._

/*
Creates a key file with a given seed for testing
with the Bitcoin Improvement Project 39 specification

Expected input phrase length is 12 words corresponding to 128 bit uuid
 */

class Bip39Spec extends FlatSpec with Matchers with Bip39 {

  "A seed phrase" should "be generated" in {
    val (seedHex,phrase) = seedPhrase(uuidString)
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
    val (seedHex,phrase) = seedPhrase(uuidString)

    Try(println(phraseToHex(phraseGood)))
    Try(println(phraseToHex(phraseBad)))
    Try(println(phraseToHex(phraseMixed)))
    Try(println(phraseToHex(phraseShort)))
    assert(phraseToHex(phraseColeman) == hexColeman)
    assert(phraseToHex(phrase) == seedHex)
  }

  "A key file" should "be generated" in {
    val phraseGood = "news excite upon nothing begin candy oblige situate figure method over tomato"
    val keyFileDir = "/tmp/scorex/test-data/keyfiles/bip39test"
    val path: Path = Path(keyFileDir)
    Try(path.deleteRecursively())
    Try(path.createDirectory())
    val password = "password"
    val (seedHex,phrase) = seedPhrase(uuidString)
    val seed = hexToUuid(seedHex)
    val seedHash: Array[Byte] = FastCryptographicHash(seed)
    val myKey = KeyFile(password, seedHash, keyFileDir)
    val sk = myKey.getPrivateKey(password)
  }

}
