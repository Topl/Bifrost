//package co.topl.crypto
//
//import co.topl.attestation.keyManagement.PrivateKeyCurve25519
//import co.topl.utils.Logging
//import org.scalatest.flatspec.AnyFlatSpec
//import org.scalatest.matchers.should.Matchers
//import co.topl.crypto.hash.Blake2b256
//
///*
// * Test class for verifying BIP39 phrase translator class
// */
//
//class Bip39Spec extends AnyFlatSpec
//  with Matchers
//  with Logging {
//
//  // sample uuid string
//  val uuidString: String = java.util.UUID.randomUUID.toString
//
//  // language for phrase words
//  val lang = "en"
//
//  //phrase translator
//  val pt: Bip39 = Bip39(lang)
//
//  "A seed phrase" should "be generated" in {
//    val (seedHex,phrase) = pt.uuidSeedPhrase(uuidString)
//    log.debug("seed hex: " + seedHex)
//    log.debug("seed phrase: " + phrase)
//  }
//
//  "A seed phrase" should "be translated to hex" in{
//    val phraseGood = "news excite upon nothing begin candy oblige situate figure method over tomato"
//    val phraseBad = "this is a bad phrase that i hope will throw an error"
//    val phraseShort = "news excite upon nothing begin candy oblige situate figure method over"
//    val phraseMixed = "excite news upon nothing begin candy oblige situate figure method over tomato"
//    val phraseColeman = "exercise crop sorry shiver jealous glue oblige evoke enrich cram air fringe"
//    val hexColeman = "4f467f3de31778c7e60a704b064415ae"
//    val (seedHex,phrase) = pt.uuidSeedPhrase(uuidString)
//
//    val phraseGood15 = "secret portion force rebuild often grow fall carbon zebra van palm bar typical enter robot"
//    val phraseGood18 = "stand earth guess employ goose aisle great next embark weapon wonder aisle monitor surface omit guilt model rule"
//    val phraseGood21 = "seven army trash viable rude ignore other arena dove wood dynamic gift broken lunch glue yellow isolate crawl damage old ripple"
//    val phraseGood24 = "siege earth jaguar gallery mom fuel unlock mimic flush develop tragic cross sense inner damp drop resist pretty example october chef energy knee cable"
//
//    def checkPT(arg: String): String ={
//      val passCheckSum = pt.phraseCheckSum(arg)
//      var outString = ""
//      outString += "Seed Phrase:\n" + arg + "\n"
//      if (passCheckSum) {
//        outString += "Passed Checksum\n"
//      } else {
//        outString += "Failed Checksum\n"
//      }
//      if (passCheckSum) {
//        outString += "Hex Value: " + pt.phraseToHex(arg) + "\n\n"
//      } else {
//        outString += "Not a valid Seed Phrase\n\n"
//      }
//      outString
//    }
//
//    log.debug(checkPT(phraseGood))
//    log.debug(checkPT(phraseBad))
//    log.debug(checkPT(phraseShort))
//    log.debug(checkPT(phraseMixed))
//
//    log.debug(checkPT(phraseGood15))
//    log.debug(checkPT(phraseGood18))
//    log.debug(checkPT(phraseGood21))
//    log.debug(checkPT(phraseGood24))
//
//
//    pt.phraseToHex(phraseColeman) shouldEqual hexColeman
//    pt.phraseToHex(phrase) shouldEqual seedHex
//  }
//
//  "A key file" should "be generated" in {
//    val (seedHex,phrase) = pt.uuidSeedPhrase(uuidString)
//    val seed1 = pt.hexToUuid(seedHex)
//    val seed2 = pt.hexToUuid(pt.phraseToHex(phrase))
//    val seed1Hash: Array[Byte] = Blake2b256(seed1)
//    val seed2Hash: Array[Byte] = Blake2b256(seed2)
//    val key1 = PrivateKeyCurve25519.secretGenerator.generateSecret(seed1Hash)
//    val key2 = PrivateKeyCurve25519.secretGenerator.generateSecret(seed2Hash)
//    val key3 = PrivateKeyCurve25519.secretGenerator.generateSecret(Blake2b256(uuidString))
//
//    key1 shouldEqual key2
//    key2 shouldEqual key3
//    key1 shouldEqual key3
//  }
//}
