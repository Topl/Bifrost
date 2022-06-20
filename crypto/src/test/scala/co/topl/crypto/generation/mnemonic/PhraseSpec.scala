package co.topl.crypto.generation.mnemonic

import co.topl.crypto.generation.mnemonic.Language.LanguageWordList

class PhraseSpec {}

object PhraseTest {

  def main(args: Array[String]): Unit = {
    val phraseString = "legal winner thank year wave sausage worth useful legal winner thank yellow"
    val size = MnemonicSizes.`12`
    val wordList = LanguageWordList.validated(Language.English).getOrElse(throw new Exception("error getting wordlist"))
    val phrase = Phrase.validated(phraseString, size, wordList).getOrElse(throw new Exception("error getting phrase"))
    val wordListIndex = phrase.value.map(wordList.value.indexOf(_))
    val bitWordList = wordListIndex.map(intTo11BitString)
    val phraseBinaryString = Phrase.toBinaryString(phrase)
    val checksum = Phrase.calculateChecksum(phraseBinaryString._1, size)

    println(
      s"phrase: $phrase\n " +
      s"phraseBinaryString: $phraseBinaryString\n " +
      s"checksum: $checksum\n " +
      s"word list index: $wordListIndex\n " +
      s"word list indices map to 11bits: $bitWordList\n "
    )
  }
}
