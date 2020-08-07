package bifrost.crypto

import bifrost.crypto.KeyFile.uuid

import scala.reflect.io.Path
import scala.util.Try
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/*
 * Test class for verifying BIP39 phrase translator class
 */

class Bip39Spec extends AnyFlatSpec with Matchers {

  val keyFileDir = "/tmp/bifrost/test-data/keyfiles/bip39test"
  val path: Path = Path(keyFileDir)

  // sample uuid string
  val uuidString = uuid
  // language for phrase words
  val lang = "en"
  //phrase translator
  val pt = Bip39.apply(lang)

  "The wordlists" should "pass checksum" in {
    assert(pt.verifyPhraseList)
  }

  "A seed phrase" should "be generated" in {
    val (seedHex,phrase) = pt.uuidSeedPhrase(uuidString)
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
    val (seedHex,phrase) = pt.uuidSeedPhrase(uuidString)

    val phraseGood15 = "secret portion force rebuild often grow fall carbon zebra van palm bar typical enter robot"
    val phraseGood18 = "stand earth guess employ goose aisle great next embark weapon wonder aisle monitor surface omit guilt model rule"
    val phraseGood21 = "seven army trash viable rude ignore other arena dove wood dynamic gift broken lunch glue yellow isolate crawl damage old ripple"
    val phraseGood24 = "siege earth jaguar gallery mom fuel unlock mimic flush develop tragic cross sense inner damp drop resist pretty example october chef energy knee cable"

    def checkPT(arg: String): String ={
      val passCheckSum = pt.phraseCheckSum(arg)
      var outString = ""
      outString += "Seed Phrase:\n" + arg + "\n"
      if (passCheckSum) {
        outString += "Passed Checksum\n"
      } else {
        outString += "Failed Checksum\n"
      }
      if (passCheckSum) {
        outString += "Hex Value: " + pt.phraseToHex(arg) + "\n\n"
      } else {
        outString += "Not a valid Seed Phrase\n\n"
      }
      outString
    }

    println(checkPT(phraseGood))
    println(checkPT(phraseBad))
    println(checkPT(phraseShort))
    println(checkPT(phraseMixed))

    println(checkPT(phraseGood15))
    println(checkPT(phraseGood18))
    println(checkPT(phraseGood21))
    println(checkPT(phraseGood24))


    assert(pt.phraseToHex(phraseColeman) == hexColeman)

    assert(pt.phraseToHex(phrase) == seedHex)
  }

  "A key file" should "be generated" in {
    Try(path.deleteRecursively())
    Try(path.createDirectory())
    val password = "password"
    val (seedHex,phrase) = pt.uuidSeedPhrase(uuidString)
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

  Try(path.deleteRecursively())
}
