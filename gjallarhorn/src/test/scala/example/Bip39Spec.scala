package example

import keymanager.{Bip39, KeyFile}
import keymanager.KeyFile.uuid
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.reflect.io.Path
import scala.util.Try
import scorex.crypto.hash.Blake2b256

class Bip39Spec extends AnyFlatSpec with Matchers {

  val keyFileDir = "/tmp/gjallarhorn/test-data/keyfiles/bip39test"
  val path: Path = Path(keyFileDir)

  // sample uuid string
  val uuidString = uuid
  // language for phrase words
  val lang = "en"
  //phrase translator
  val pt = Bip39.apply(lang)
  //------------------------------------------------------------------------------------

  "A seed phrase" should "be generated" in {
    val (seedHex,phrase) = pt.uuidSeedPhrase(uuidString)
  }
  //------------------------------------------------------------------------------------

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

    assert(pt.phraseToHex(phraseColeman) == hexColeman)

    assert(pt.phraseToHex(phrase) == seedHex)
  }
  //------------------------------------------------------------------------------------
  "A key file" should "be generated" in {
    Try(path.deleteRecursively())
    Try(path.createDirectory())
    val password = "password"
    val (seedHex,phrase) = pt.uuidSeedPhrase(uuidString)
    val seed1 = pt.hexToUuid(seedHex)
    val seed2 = pt.hexToUuid(pt.phraseToHex(phrase))
    val seed1Hash: Array[Byte] = Blake2b256.hash(seed1)
    val seed2Hash: Array[Byte] = Blake2b256.hash(seed2)
    val key1 = KeyFile(password, seed1Hash, keyFileDir)
    val key2 = KeyFile(password, seed2Hash, keyFileDir)
    val key3 = KeyFile(password = password, seed = Blake2b256.hash(uuidString),defaultKeyDir = keyFileDir)
    assert(key1.getPrivateKey(password).get.privKeyBytes.mkString("")
      == key2.getPrivateKey(password).get.privKeyBytes.mkString(""))
    assert(key2.getPrivateKey(password).get.privKeyBytes.mkString("")
      == key3.getPrivateKey(password).get.privKeyBytes.mkString(""))
    assert(key3.getPrivateKey(password).get.privKeyBytes.mkString("")
      == key1.getPrivateKey(password).get.privKeyBytes.mkString(""))
  }

  Try(path.deleteRecursively())
}
