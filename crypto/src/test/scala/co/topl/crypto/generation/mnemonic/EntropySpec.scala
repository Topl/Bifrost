package co.topl.crypto.generation.mnemonic

import cats.implicits._
import co.topl.crypto.utils.{Generators, Hex, TestVector}
import io.circe.{Decoder, DecodingFailure, HCursor}
import io.circe.generic.semiauto.deriveDecoder
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class EntropySpec extends AnyPropSpec with ScalaCheckPropertyChecks with ScalaCheckDrivenPropertyChecks {

  property("random byte arrays (of the correct length) should be a valid Entropy") {
    forAll(Gen.oneOf(Seq(16, 20, 24, 28, 32))) { byteLength =>
      val bytes = Generators.genByteArrayOfSize(byteLength).sample.get

      Entropy.fromBytes(bytes).isRight shouldBe true
    }
  }

  property("Entropy derived from UUIDs should result in valid mnemonic strings") {
    forAll(Gen.uuid) { uuidGen =>
      val entropy = Entropy.fromUuid(uuidGen)

      Entropy.toMnemonicString(entropy).isRight shouldBe true
    }
  }

  property(s"Entropy can be generated and results in valid mnemonic strings") {
    forAll(Gen.posNum[Int]) { size =>
      val mnemonicSize = Generators.pickMnemonicSize(size)
      val entropy = Entropy.generate(mnemonicSize)

      Entropy.toMnemonicString(entropy).isRight shouldBe true
    }
  }

  property(
    s"Entropy can be generated, transformed to a mnemonic phrase string, and converted back to the original entropy value"
  ) {
    forAll(Gen.posNum[Int]) { size =>
      val mnemonicSize = Generators.pickMnemonicSize(size)
      val entropy1 = Entropy.generate(mnemonicSize)
      val entropy2 = Entropy
        .toMnemonicString(entropy1, Language.English)
        .flatMap { mnemonicString =>
          Entropy.fromMnemonicString(mnemonicString, mnemonicSize, Language.English)
        }
        .getOrElse(throw new Exception("again not sure :/"))

      (entropy1.value sameElements entropy2.value) shouldBe true
    }
  }

  EntropyTestVectorHelper.testVectors.foreach { underTest =>
    property(s"Test vector mnemonic should produce known entropy. Mnemonic: ${underTest.inputs.mnemonic}") {
      val actualEntropy =
        Entropy
          .fromMnemonicString(underTest.inputs.mnemonic, underTest.inputs.size, Language.English)
          .getOrElse(throw new Exception("another bad place"))

      val expectedEntropy = underTest.outputs.entropy

      (actualEntropy.value sameElements expectedEntropy.value) shouldBe true
    }
  }

  object EntropyTestVectorHelper {
    case class SpecInputs(mnemonic: String, size: MnemonicSize)
    case class SpecOutputs(entropy: Entropy)
    case class MnemonicToEntropyTestVector(inputs: SpecInputs, outputs: SpecOutputs) extends TestVector

    implicit val inputsDecoder: Decoder[SpecInputs] = (c: HCursor) =>
      for {
        mnemonic <- c.downField("mnemonic").as[String]
        wordLength = mnemonic.split(" ").length
        entropyByteLength = (wordLength * 4) / 3
        size <- Entropy
          .getMnemonicSize(entropyByteLength)
          .leftMap(err => DecodingFailure(err.toString, c.history))
      } yield SpecInputs(mnemonic, size)

    implicit val outputsDecoder: Decoder[SpecOutputs] = (c: HCursor) =>
      for {
        bytes   <- c.downField("entropy").as[String].map(Hex.decode)
        entropy <- Entropy.fromBytes(bytes).leftMap(err => DecodingFailure(err.toString, c.history))
      } yield SpecOutputs(entropy)

    implicit val testVectorDecoder: Decoder[MnemonicToEntropyTestVector] = deriveDecoder[MnemonicToEntropyTestVector]
    val testVectors: List[MnemonicToEntropyTestVector] = TestVector.read("MnemonicToEntropy.json")
  }
}
