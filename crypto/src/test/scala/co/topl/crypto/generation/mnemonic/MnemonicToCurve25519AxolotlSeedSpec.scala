package co.topl.crypto.generation.mnemonic

import co.topl.crypto.generation.mnemonic.EntropySupport.arbitraryEntropy
import co.topl.crypto.generation.mnemonic.Language.{English, LanguageWordList}
import co.topl.crypto.generation.mnemonic.MnemonicSize.{Mnemonic12, Mnemonic15}
import co.topl.crypto.signing.EntropyToSeed.instances._
import co.topl.crypto.signing.{Curve25519, EntropyToSeed}
import co.topl.crypto.utils
import co.topl.crypto.utils.Hex.implicits._
import co.topl.crypto.utils.TestVector
import co.topl.models.utility.{Lengths, Sized}
import co.topl.models.{Bytes, SecretKeys}
import io.circe.generic.semiauto.deriveDecoder
import io.circe.{parser, Decoder}
import cats.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scodec.bits.ByteVector

import java.nio.file.{Files, Paths}

class MnemonicToCurve25519AxolotlSeedSpec
    extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with EitherValues {

  case class SpecIn(entropy: Entropy, password: String)
  case class SpecOut(seed: Sized.Strict[Bytes, SecretKeys.Curve25519.Length])

  case class Curve25519AxolotlTestVector(entropy: String, mnemonics: String, passphrase: String, seed: String)
      extends TestVector

  implicit val testVectorDecoder: Decoder[Curve25519AxolotlTestVector] = deriveDecoder[Curve25519AxolotlTestVector]

  private val curveEntropy = implicitly[EntropyToSeed[SecretKeys.Curve25519.Length]]
  private val entropyToSeed = EntropyToSeed.instances.pbkdf2Sha512[Lengths.`32`.type]

  val testVectors: List[Curve25519AxolotlTestVector] =
    utils.readTestVectors("Curve25519AxolotlTestVectors.json")

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

    println(ByteVector(entropy.value))
    println(curveEntropy.toSeed(specIn.entropy, Some(specIn.password)))
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

  property("entropy and passphrase should generate a valid deterministic Topl Curve25519 Axolotl seed") {
    testVectors.foreach { vec =>
      val entropy = Entropy.validated(vec.entropy.getBytes, Mnemonic12)

      curveEntropy.toSeed(entropy.value, Some(vec.passphrase)) shouldBe vec.seed
        .unsafeStrictBytes[SecretKeys.Curve25519.Length]
    }
  }

  property("mnemonic and passphrase should generate a valid deterministic Topl Curve25519 Axolotl seed") {
    testVectors.foreach { vec =>
//      val mnemonicSize = vec.mnemonics.split(" ").length match {
//        case Mnemonic12.wordLength => Mnemonic12
////        case Mnemonic15.wordLength => MnemonicSize(15)
//      }
      val mnemonic = Phrase.validated(vec.mnemonics, Mnemonic12, wordList)
      val entropy = mnemonic.map(Entropy.fromPhrase(_, wordList, Mnemonic12))

      println(ByteVector(entropy.value.value))
      println(vec.seed)
      println(curveEntropy.toSeed(entropy.value, Some(vec.passphrase)))

      curveEntropy.toSeed(entropy.value, Some(vec.passphrase)) shouldBe vec.seed
        .unsafeStrictBytes[SecretKeys.Curve25519.Length]
    }
  }
}
