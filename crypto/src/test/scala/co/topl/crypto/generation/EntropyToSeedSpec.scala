package co.topl.crypto.generation

import cats.scalatest.EitherValues
import co.topl.crypto.generation.mnemonic.{Entropy, EntropyTestVectorHelper}
import co.topl.crypto.utils.{Hex, TestVector}
import co.topl.protobuf.Bytes
import co.topl.protobuf.utility.Lengths
import io.circe.generic.semiauto.deriveDecoder
import io.circe.{Decoder, HCursor}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class EntropyToSeedSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with EitherValues {

  case class SpecInputs(entropy: Entropy, password: Option[String])
  case class SpecOutputs(seed96: Array[Byte])
  case class EntropyToSeedTestVectors(inputs: SpecInputs, outputs: SpecOutputs) extends TestVector

  implicit val inputsDecoder: Decoder[SpecInputs] = (c: HCursor) =>
    for {
      entropy  <- EntropyTestVectorHelper.entropyDecoder(c)
      password <- c.downField("password").as[Option[String]]
    } yield SpecInputs(entropy, password)

  implicit val outputsDecoder: Decoder[SpecOutputs] = (c: HCursor) =>
    for {
      bytes <- c.downField("seed96").as[String].map(Hex.decode)
      specOut = SpecOutputs(bytes)
    } yield specOut

  implicit val testVectorDecoder: Decoder[EntropyToSeedTestVectors] = deriveDecoder[EntropyToSeedTestVectors]

  val testVectors: List[EntropyToSeedTestVectors] = TestVector.read("generation/EntropyToSeed.json")

  testVectors.foreach { underTest =>
    property(s"Generate 96 byte seed from entropy: ${underTest.inputs.entropy}") {
      val actualSeed =
        EntropyToSeed.instances
          .pbkdf2Sha512[Lengths.`96`.type]
          .toSeed(underTest.inputs.entropy, underTest.inputs.password)
          .data

      val expectedSeed = Bytes(underTest.outputs.seed96)

      (actualSeed == expectedSeed) shouldBe true

    }
  }
}
