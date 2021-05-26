package co.topl.attestation.keyManagement

import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.implicits.identityBytesEncoder
import co.topl.utils.encode.Base16
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Bip39Spec extends AnyPropSpec with CommonGenerators {
  property("valid 12 phrase mnemonic is valid input phrase") {
    val mnemonic = "cat swing flag economy stadium alone churn speed unique patch report train"
    val englishBip39 = Bip39.withLanguage(Bip39.English).getOrElse(throw new Error("Failed to read english word list"))

    englishBip39.validateInputPhrase(mnemonic) shouldBe true
  }

  property("invalid 12 phrase mnemonic is invalid input phrase") {
    val mnemonic = "amber glue hallway can truth drawer wave flex cousin grace close compose"
    val englishBip39 = Bip39.withLanguage(Bip39.English).getOrElse(throw new Error("Failed to read english word list"))

    englishBip39.validateInputPhrase(mnemonic) shouldBe false
  }

  property("phrase should always output the same seed") {
    val mnemonic = "ozone drill grab fiber curtain grace pudding thank cruise elder eight picnic"
    val englishBip39 = Bip39.withLanguage(Bip39.English).getOrElse(throw new Error("Failed to read english word list"))
    val password = "test"

    val firstAttempt = englishBip39.phraseToSeed(mnemonic, password)
    val secondAttempt = englishBip39.phraseToSeed(mnemonic, password)

    firstAttempt shouldBe secondAttempt
  }

  property("test vectors should pass") {
    case class Bip39TestVector(mnemonic: String, password: String, seed: String)

    val testVectors = Seq(
      Bip39TestVector(
        "hand chicken express snow under choice cruise pumpkin follow edge myth sunset",
        "topl",
        "b7361dfc9f54f4f35bf5ca868625f6e834089320e635987f73a0c8ff7fc04f1f35bc3be1b7e09da7fd9bc6ae26e61fc9aaf7899bb2a1faf2386d3e6f9d7e49e7"
      )
    )

    val englishBip39 = Bip39.withLanguage(Bip39.English).getOrElse(throw new Error("Failed to read english word list"))

    testVectors.foreach { vector =>
      val seed = englishBip39.phraseToSeed(vector.mnemonic, vector.password)

      Base16.encode(seed) shouldBe vector.seed
    }
  }
}
