package co.topl.attestation.keyManagement

import co.topl.crypto.hash.implicits._
import co.topl.crypto.hash.Blake2b
import co.topl.crypto.hash.digest.Digest64
import co.topl.utils.CommonGenerators
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

    val firstAttempt = englishBip39.phraseToSeed[Blake2b, Digest64](mnemonic, password)
    val secondAttempt = englishBip39.phraseToSeed[Blake2b, Digest64](mnemonic, password)

    firstAttempt shouldBe secondAttempt
  }
}
