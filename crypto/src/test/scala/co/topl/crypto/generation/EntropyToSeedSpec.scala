package co.topl.crypto.generation

import cats.scalatest.EitherValues
import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.signing.EntropyToSeed
import co.topl.crypto.utils
import co.topl.crypto.utils.TestVector
import co.topl.models.Bytes
import co.topl.models.utility.{Lengths, Sized}
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class EntropyToSeedSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with EitherValues {

  case class SpecInputs(entropy: String)
  case class SpecOutputs(seed32: String, seed64: String, seed96: String)
  case class EntropyToSeedTestVectors(inputs: SpecInputs, outputs: SpecOutputs) extends TestVector

  implicit val inputsDecoder: Decoder[SpecInputs] = deriveDecoder[SpecInputs]
  implicit val outputsDecoder: Decoder[SpecOutputs] = deriveDecoder[SpecOutputs]
  implicit val testVectorDecoder: Decoder[EntropyToSeedTestVectors] = deriveDecoder[EntropyToSeedTestVectors]

  val testVectors: List[EntropyToSeedTestVectors] = TestVector.read("EntropyToSeedTestVectors.json")

  property("Generate 32 byte length seeds") {
    testVectors.foreach { vec =>
      val entropy = Entropy(vec.inputs.entropy.getBytes)

      EntropyToSeed.instances.pbkdf2Sha512(Lengths.`32`).toSeed(entropy, Some("TREZOR")) shouldBe vec.outputs.seed32
    }
  }

  property("Generate 64 byte length seeds") {
    testVectors.foreach { vec =>
      val entropy = Entropy(vec.inputs.entropy.getBytes)

      EntropyToSeed.instances.pbkdf2Sha512(Lengths.`64`).toSeed(entropy, Some("TREZOR")) shouldBe vec.outputs.seed64
    }
  }

  property("Generate 96 byte length seeds") {
    testVectors.foreach { vec =>
      val entropy = Entropy(vec.inputs.entropy.getBytes)

      EntropyToSeed.instances.pbkdf2Sha512(Lengths.`96`).toSeed(entropy, Some("TREZOR")) shouldBe vec.outputs.seed96
    }
  }
}
