package bifrost.crypto

<<<<<<< HEAD
import bifrost.keygen.KeyFile
import bifrost.crypto.hash.FastCryptographicHash
import scala.reflect.io.Path
import scala.util.Try
import org.scalatest._
=======
import bifrost.crypto.hash.FastCryptographicHash
import bifrost.keygen.KeyFile
import scala.io.Source
import bifrost.keygen.KeyFile.uuid
import scorex.crypto.hash.Sha256
import scorex.crypto.encode.Base58
import scala.reflect.io.Path
import scala.util.Try
import org.scalatest._
import scala.math.BigInt
>>>>>>> ee14f1128a0f14ef46f61f6681d531e311cec662

/*
Creates a key file with a given seed for testing
with the Bitcoin Improvement Project 39 specification

Expected input phrase length is 12 words corresponding to 128 bit uuid
 */

<<<<<<< HEAD
class Bip39Spec extends FlatSpec with Matchers with Bip39 {

  "A seed phrase" should "be generated" in {
    val (seedHex,phrase) = seedPhrase(uuidString)
    println("seed hex: "+seedHex)
=======
class Bip39Spec extends FlatSpec with Matchers{

  val lang = "english.txt"
  val wordListDir = "src/main/resources/bip-0039/"
  val wordList = Source.fromFile(wordListDir + lang).getLines.toList
  val uuidString = uuid

  def toBinaryIndex(i: Int): String = String.format("%11s", BigInt(i).toString(2) ).replace(' ', '0')
  def toBinaryByte(b: Byte): String = String.format("%8s", BigInt(b & 0xff).toString(2) ).replace(' ', '0')
  def hexToUuid(s: String) : String = s.slice(0,8)+"-"+s.slice(8,12)+"-"+s.slice(12,16)+"-"+s.slice(16,20)+"-"+s.substring(20)
  def phraseToHex(phrase: String): String = {
    val phraseWords: Array[String] = phrase.split(" ")
    val wordChk: Boolean = phraseWords.map(wordList.contains(_)).foldLeft(true)(_ && _) && phraseWords.length == 12
    if (!wordChk) throw new Exception("Seed phrase doesn't adhere to BIP39")
    val phraseIndex: Array[Int] = phraseWords.map(wordList.indexOf(_))
    val phraseBin = phraseIndex.map(toBinaryIndex(_)).mkString
    val phraseBytes: Array[Byte] = phraseBin.slice(0,128).grouped(8).toArray map {Integer.parseInt(_, 2).toByte}
    val chksum = phraseBin.substring(128)
    val phraseHash: Array[Byte] = Sha256.hash(phraseBytes)
    val phraseHashBin: Array[String] = phraseHash.map(toBinaryByte(_))
    if (chksum != phraseHashBin(0).slice(0,4)) throw new Exception("Seed phrase doesn't pass checksum")
    val phraseHex = phraseBytes.map("%02x" format _).mkString
    return phraseHex
  }

  def seedPhrase(inputUuid: String): (String,String) = {
    val toRemove = "-".toSet
    val seed = inputUuid.filterNot(toRemove)
    val seedBytes: Array[Byte] = seed.grouped(2).toArray map {Integer.parseInt(_, 16).toByte}
    val seedHash: Array[Byte] = Sha256.hash(seedBytes)
    val seedBin: Array[String] = seedBytes.map(toBinaryByte(_))
    val seedHashBin: Array[String] = seedHash.map(toBinaryByte(_))
    val chksum = seedHashBin(0).slice(0,4)
    val seedPhraseBin = seedBin.mkString("") + chksum
    val phraseBin: Array[String] = seedPhraseBin.grouped(11).toArray
    val phrase = phraseBin.map(Integer.parseInt(_,2)).map(wordList(_)).mkString(" ")
    return (seed,phrase)
  }

  "A seed phrase" should "be generated" in {
    val (seed,phrase) = seedPhrase(uuidString)
    println(uuidString)
    println(hexToUuid(seed))
    println("seed hex: "+seed)
>>>>>>> ee14f1128a0f14ef46f61f6681d531e311cec662
    println("seed phrase: "+phrase)
  }

  "A seed phrase" should "be translated to hex" in{
    val phraseGood = "news excite upon nothing begin candy oblige situate figure method over tomato"
    val phraseBad = "this is a bad phrase that i hope will throw an error"
    val phraseShort = "news excite upon nothing begin candy oblige situate figure method over"
    val phraseMixed = "excite news upon nothing begin candy oblige situate figure method over tomato"
    val phraseColeman = "exercise crop sorry shiver jealous glue oblige evoke enrich cram air fringe"
    val hexColeman = "4f467f3de31778c7e60a704b064415ae"
<<<<<<< HEAD
    val (seedHex,phrase) = seedPhrase(uuidString)
=======
>>>>>>> ee14f1128a0f14ef46f61f6681d531e311cec662

    Try(println(phraseToHex(phraseGood)))
    Try(println(phraseToHex(phraseBad)))
    Try(println(phraseToHex(phraseMixed)))
    Try(println(phraseToHex(phraseShort)))
    assert(phraseToHex(phraseColeman) == hexColeman)
<<<<<<< HEAD
    assert(phraseToHex(phrase) == seedHex)
  }

  "A key file" should "be generated" in {
    val phraseGood = "news excite upon nothing begin candy oblige situate figure method over tomato"
=======
  }

  "A key file" should "be generated" in {
>>>>>>> ee14f1128a0f14ef46f61f6681d531e311cec662
    val keyFileDir = "/tmp/scorex/test-data/keyfiles/bip39test"
    val path: Path = Path(keyFileDir)
    Try(path.deleteRecursively())
    Try(path.createDirectory())
    val password = "password"
<<<<<<< HEAD
    val (seedHex,phrase) = seedPhrase(uuidString)
    val seed = hexToUuid(seedHex)
=======
    val seed = uuidString
>>>>>>> ee14f1128a0f14ef46f61f6681d531e311cec662
    val seedHash: Array[Byte] = FastCryptographicHash(seed)
    val myKey = KeyFile(password, seedHash, keyFileDir)
    val sk = myKey.getPrivateKey(password)
  }

}
