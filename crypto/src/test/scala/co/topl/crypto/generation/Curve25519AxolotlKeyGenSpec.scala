package co.topl.crypto.generation

import co.topl.crypto.generation.mnemonic.EntropySupport.arbitraryEntropy
import co.topl.crypto.generation.mnemonic.Language.{English, LanguageWordList}
import co.topl.crypto.generation.mnemonic.MnemonicSize.Mnemonic12
import co.topl.crypto.generation.mnemonic.{Entropy, Phrase}
import co.topl.crypto.utils.Hex.implicits._
import co.topl.crypto.signing.EntropyToSeed.instances._
import co.topl.crypto.signing.{Curve25519, EntropyToSeed}
import co.topl.models.utility.Sized
import co.topl.models.{Bytes, SecretKeys}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scodec.bits.ByteVector

class Curve25519AxolotlKeyGenSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  case class SpecIn(entropy: Entropy, password: String)
  case class SpecOut(seed: Sized.Strict[Bytes, SecretKeys.Curve25519.Length])

  private val curveEntropy = implicitly[EntropyToSeed[SecretKeys.Curve25519.Length]]

  private val wordList = LanguageWordList.validated(English) match {
    case Left(err)   => throw new Exception(s"Could not load English language BIP-0039 file: $err")
    case Right(list) => list
  }

  private def unsafePhrase(words: String): Phrase =
    Phrase.unsafeWrap(words.toLowerCase.split("\\s+").map(_.trim).toIndexedSeq)

  property("with Curve25519, keyPairs generated with the same seed should be the same") {
    forAll { entropy: Entropy =>
      whenever(entropy.value.nonEmpty) {
        val curve25519 = new Curve25519

        val (sk1, vk1) = curve25519.createKeyPair(entropy, None)
        val (sk2, vk2) = curve25519.createKeyPair(entropy, None)

        sk1 === sk2 shouldBe true
        vk1 === vk2 shouldBe true
      }
    }
  }

  property("mnemonic should generate a seed from test vector 1") {
    val phrase =
      unsafePhrase("abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about")
    val entropy = Entropy.fromPhrase(phrase, wordList, Mnemonic12)
    val specIn = SpecIn(entropy, "TREZOR")
    val specOut = SpecOut("9f49b8aa6610995af06dd77f4c73866fba249f398ed9fe2327726d12b4e71cad".unsafeStrictBytes)

    curveEntropy.toSeed(specIn.entropy, Some(specIn.password)) shouldBe specOut.seed
  }

  property("mnemonic should generate a seed from test vector 2") {
    val phrase =
      unsafePhrase("legal winner thank year wave sausage worth useful legal winner thank yellow")
    val entropy = Entropy.fromPhrase(phrase, wordList, Mnemonic12)
    val specIn = SpecIn(entropy, "TREZOR")
    val specOut = SpecOut("d0e00d9e9c21e3d8c8e5002b175691e09546983d4472a97b49fa2fb3dfa01040".unsafeStrictBytes)

    println(ByteVector(entropy.value))
    println(curveEntropy.toSeed(specIn.entropy, Some(specIn.password)))
    curveEntropy.toSeed(specIn.entropy, Some(specIn.password)) shouldBe specOut.seed
  }

  property("mnemonic should generate a seed from test vector 3") {
    val phrase =
      unsafePhrase("letter advice cage absurd amount doctor acoustic avoid letter advice cage above")
    val entropy = Entropy.fromPhrase(phrase, wordList, Mnemonic12)
    val specIn = SpecIn(entropy, "TREZOR")
    val specOut = SpecOut("5e308762cbb38193ff709e7817668d88dc641cec0d06a475e497c482b0982d25".unsafeStrictBytes)

    println(ByteVector(entropy.value))
    println(curveEntropy.toSeed(specIn.entropy, Some(specIn.password)))
    curveEntropy.toSeed(specIn.entropy, Some(specIn.password)) shouldBe specOut.seed
  }

}
