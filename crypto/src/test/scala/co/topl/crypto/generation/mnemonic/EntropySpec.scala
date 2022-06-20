package co.topl.crypto.generation.mnemonic

class EntropySpec {}

object EntropyTest {

  def main(args: Array[String]): Unit = {
    val size = MnemonicSizes.`24`
    val entropy = Entropy.generate(size)
    val mnemonicString = Entropy.toMnemonicString(entropy, size, Language.English) match {
      case Left(value)  => throw new Exception(s"$value")
      case Right(value) => value
    }
    val entropy2 = Entropy.fromMnemonicString(
      mnemonicString,
      size,
      Language.English
    )

    println(mnemonicString)

    Phrase
      .fromEntropy(
        entropy2.getOrElse(throw new Exception("error in Phrase from entropy 2")),
        size,
        Language.English
      )
      .map(p => println(p.value))
  }
}
