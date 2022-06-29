package co.topl.crypto.generation

import cats.scalatest.EitherValues
import co.topl.crypto.generation.mnemonic._
import co.topl.crypto.signing.{Curve25519, Ed25519, Ed25519VRF, ExtendedEd25519}
import co.topl.crypto.utils.TestVector
import co.topl.models.SecretKeys
import io.circe.generic.semiauto.deriveDecoder
import io.circe.{Decoder, HCursor}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

/**
 * test vectors adapted from multiple sources:
 * https://github.com/cardano-foundation/CIPs/blob/master/CIP-0003/Icarus.md#test-vectors
 * https://github.com/input-output-hk/rust-cardano/blob/9fad3d12341acc2ab0f9c2026149af3d839447e4/cardano/src/bip/test_vectors/bip39_english.txt
 */

class KeyInitializerSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with EitherValues {
  import KeyInitializer.Instances._
  implicit val curve25519Instance: Curve25519 = new Curve25519
  implicit val ed25519Instance: Ed25519 = new Ed25519
  implicit val vrfEd25519Instance: Ed25519VRF = new Ed25519VRF
  implicit val extendedEd25519Instance: ExtendedEd25519 = new ExtendedEd25519

  case class SpecInputs(mnemonic: String, size: MnemonicSize, password: Option[String])

  case class SpecOutputs(
    curve25519:      SecretKeys.Curve25519,
    ed25519:         SecretKeys.Ed25519,
    vrfEd25519:      SecretKeys.VrfEd25519,
    extendedEd25519: SecretKeys.ExtendedEd25519
  )
  case class KeyInitializorTestVector(inputs: SpecInputs, outputs: SpecOutputs) extends TestVector

  implicit val inputsDecoder: Decoder[SpecInputs] = (c: HCursor) =>
    for {
      (mnemonicString, size) <- EntropyTestVectorHelper.mnemonicStringAndSize(c)
      password               <- c.downField("password").as[Option[String]]
    } yield SpecInputs(mnemonicString, size, password)

  implicit val outputsDecoder: Decoder[SpecOutputs] = (c: HCursor) =>
    for {
      curve25519 <- c.get[String]("curve25519").map(curve25519Initializer.fromBase16String(_).value)
      ed25519    <- c.get[String]("ed25519").map(ed25519Initializer.fromBase16String(_).value)
      vrfEd25519 <- c.get[String]("vrfEd25519").map(vrfInitializer.fromBase16String(_).value)
      extendedEd25519 <- c
        .get[String]("extendedEd25519")
        .map(extendedEd25519Initializer.fromBase16String(_).value)
    } yield SpecOutputs(curve25519, ed25519, vrfEd25519, extendedEd25519)

  implicit val testVectorDecoder: Decoder[KeyInitializorTestVector] = deriveDecoder[KeyInitializorTestVector]

  val testVectors: List[KeyInitializorTestVector] = TestVector.read("generation/KeyInitializer.json")

  testVectors.foreach { underTest =>
    property(
      s"Generate 96 byte seed from mnemonic: ${underTest.inputs.mnemonic} + password: ${underTest.inputs.password}"
    ) {
      val actualCurve25519Sk = curve25519Initializer
        .fromMnemonicString(underTest.inputs.mnemonic)(Language.English, underTest.inputs.password)
      val actualEd25519Sk = ed25519Initializer
        .fromMnemonicString(underTest.inputs.mnemonic)(Language.English, underTest.inputs.password)
      val actualVrf25519Sk = vrfInitializer
        .fromMnemonicString(underTest.inputs.mnemonic)(Language.English, underTest.inputs.password)
      val actualExtended25519Sk = extendedEd25519Initializer
        .fromMnemonicString(underTest.inputs.mnemonic)(Language.English, underTest.inputs.password)

      actualCurve25519Sk shouldBe underTest.outputs.curve25519
      actualEd25519Sk shouldBe underTest.outputs.ed25519
      actualVrf25519Sk shouldBe underTest.outputs.vrfEd25519
      actualExtended25519Sk shouldBe underTest.outputs.extendedEd25519

    }
  }
}
