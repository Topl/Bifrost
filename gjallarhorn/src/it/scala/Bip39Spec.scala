import scala.util.Try

class Bip39Spec extends AnyFlatSpec with Matchers {

  val keyFileDir = "/tmp/gjallarhorn/test-data/keyfiles/bip39test"
  val path: Path = Path(keyFileDir)

  // sample uuid string
  val uuidString: String = java.util.UUID.randomUUID.toString
  // language for phrase words
  val lang = "en"
  // phrase translator
  val pt = Bip39.apply(lang)
  // ------------------------------------------------------------------------------------

  "A seed phrase" should "be generated" in {
    val (seedHex, phrase) = pt.uuidSeedPhrase(uuidString)
  }
  // ------------------------------------------------------------------------------------

  "A seed phrase" should "be translated to hex" in {
    val phraseGood = "news excite upon nothing begin candy oblige situate figure method over tomato"
    val phraseBad = "this is a bad phrase that i hope will throw an error"
    val phraseShort = "news excite upon nothing begin candy oblige situate figure method over"
    val phraseMixed = "excite news upon nothing begin candy oblige situate figure method over tomato"
    val phraseColeman = "exercise crop sorry shiver jealous glue oblige evoke enrich cram air fringe"
    val hexColeman = "4f467f3de31778c7e60a704b064415ae"
    val (seedHex, phrase) = pt.uuidSeedPhrase(uuidString)

    val phraseGood15 = "secret portion force rebuild often grow fall carbon zebra van palm bar typical enter robot"
    val phraseGood18 =
      "stand earth guess employ goose aisle great next embark weapon wonder aisle monitor surface omit guilt model rule"
    val phraseGood21 =
      "seven army trash viable rude ignore other arena dove wood dynamic gift broken lunch glue yellow isolate crawl damage old ripple"
    val phraseGood24 =
      "siege earth jaguar gallery mom fuel unlock mimic flush develop tragic cross sense inner damp drop resist pretty example october chef energy knee cable"

    def checkPT(arg: String): String = {
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
  // ------------------------------------------------------------------------------------
  /*
  "A key file" should "be generated" in {
    Try(path.deleteRecursively())
    Try(path.createDirectory())
    val password = "password"
    val (seedHex,phrase) = pt.uuidSeedPhrase(uuidString)
    val seed1: String = pt.hexToUuid(seedHex)
    val seed2: String = pt.hexToUuid(pt.phraseToHex(phrase))
    val seed1Hash: Array[Byte] = Blake2b256.hash(seed1)
    val seed2Hash: Array[Byte] = Blake2b256.hash(seed2)
    val key1 = PrivateKeyCurve25519.generateKeys(seed1Hash)
    val key2 = PrivateKeyCurve25519.generateKeys(seed2Hash)
    val key3 = PrivateKeyCurve25519.generateKeys(Blake2b256.hash(uuidString))

    KeyFile.generateKeyPair(seed1Hash)

    key1 shouldEqual key2
    key2 shouldEqual key3
    key1 shouldEqual key3
  }
   */

  Try(path.deleteRecursively())
}
