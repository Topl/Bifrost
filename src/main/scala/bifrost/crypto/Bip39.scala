package bifrost.crypto

import bifrost.keygen.KeyFile.uuid
import scorex.crypto.hash.Sha256
import scala.io.Source
import scala.math.BigInt

trait Bip39 {

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
}