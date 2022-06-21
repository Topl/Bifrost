package co.topl.crypto.generation

import cats.scalatest.EitherValues
import co.topl.crypto.generation.mnemonic._
import co.topl.crypto.signing.Ed25519VRF
import co.topl.crypto.utils.{Hex, TestVector}
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Sized
import co.topl.models.{Bytes, SecretKeys}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

/**
 * test vectors adapted from multiple sources:
 * https://github.com/cardano-foundation/CIPs/blob/master/CIP-0003/Icarus.md#test-vectors
 * https://github.com/input-output-hk/rust-cardano/blob/9fad3d12341acc2ab0f9c2026149af3d839447e4/cardano/src/bip/test_vectors/bip39_english.txt
 */

class KeyInitializorSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with EitherValues {
  import KeyInitializer.Instances._

  case class SpecInputs(mnemonic: String, size: MnemonicSize, password: Option[String])

  case class SpecOutputs(
    curve25519:      SecretKeys.Curve25519,
    ed25519:         SecretKeys.Ed25519,
    vrfEd25519:      SecretKeys.VrfEd25519,
    extendedEd25519: SecretKeys.ExtendedEd25519
  )
  case class KeyInitializor(inputs: SpecInputs, outputs: SpecOutputs) extends TestVector

  implicit val inputsDecoder: Decoder[SpecInputs] = (c: HCursor) =>
    for {
      (mnemonicString, size) <- EntropyTestVectorHelper.mnemonicStringAndSize(c)
      password               <- c.downField("password").as[Option[String]]
    } yield SpecInputs(mnemonicString, size, password)

  private def decodeHexStringToSK[SK](
    c:        HCursor,
    key:      String,
    createSk: Array[Byte] => SK
  ): Either[DecodingFailure, SK] =
    for {
      secretKey <- c
        .get[String](key)
        .map(Hex.decode)
        .map(createSk)
    } yield secretKey

  implicit val outputsDecoder: Decoder[SpecOutputs] = (c: HCursor) =>
    for {
      curve25519 <- decodeHexStringToSK(c, "curve25519", b => SecretKeys.Curve25519(Sized.strictUnsafe(Bytes(b))))
      ed25519    <- decodeHexStringToSK(c, "ed25519", b => SecretKeys.Ed25519(Sized.strictUnsafe(Bytes(b))))
      vrfEd25519 <- decodeHexStringToSK(c, "vrfEd25519", b => SecretKeys.VrfEd25519(Sized.strictUnsafe(Bytes(b))))
      extendedEd25519 <- decodeHexStringToSK(
        c,
        "extendedEd25519",
        b =>
          SecretKeys.ExtendedEd25519(
            Sized.strictUnsafe(Bytes(b.slice(0, 32))),
            Sized.strictUnsafe(Bytes(b.slice(32, 64))),
            Sized.strictUnsafe(Bytes(b.slice(64, 96)))
          )
      )
    } yield SpecOutputs(curve25519, ed25519, vrfEd25519, extendedEd25519)

  implicit val testVectorDecoder: Decoder[KeyInitializor] = deriveDecoder[KeyInitializor]

  val testVectors: List[KeyInitializor] = TestVector.read("KeyInitializor.json")

  testVectors.foreach { underTest =>
    property(
      s"Generate 96 byte seed from mnemonic: ${underTest.inputs.mnemonic} + password: ${underTest.inputs.password}"
    ) {
      val entropy = Entropy.fromMnemonicString(underTest.inputs.mnemonic, underTest.inputs.size, Language.English).value
      val actualCurve25519Sk = curve25519Initializer.fromEntropy(entropy, underTest.inputs.password)
      val actualEd25519Sk = ed25519Initializer.fromEntropy(entropy, underTest.inputs.password)
      val actualVrf25519Sk = vrfInitializer(new Ed25519VRF).fromEntropy(entropy, underTest.inputs.password)
      val actualExtended25519Sk = extendedEd25519Initializer.fromEntropy(entropy, underTest.inputs.password)

      actualCurve25519Sk shouldBe underTest.outputs.curve25519
      actualEd25519Sk shouldBe underTest.outputs.ed25519
      actualVrf25519Sk shouldBe underTest.outputs.vrfEd25519
      actualExtended25519Sk shouldBe underTest.outputs.extendedEd25519

    }
  }
}

object GenSecrets {
  import KeyInitializer.Instances._

  case class SpecOutputs(
    curve25519:      SecretKeys.Curve25519,
    ed25519:         SecretKeys.Ed25519,
    vrfEd25519:      SecretKeys.VrfEd25519,
    extendedEd25519: SecretKeys.ExtendedEd25519
  )

  def main(args: Array[String]): Unit = {
    val mnemonicString =
      "vessel erase town arrow girl emotion siren better fork approve spare convince sauce amused clap"
    val password = "heart"

    val entropy = Entropy
      .fromMnemonicString(mnemonicString, MnemonicSizes.`15`, Language.English)
      .getOrElse(throw new Exception("oops"))
    val actualCurve25519Sk = curve25519Initializer.fromEntropy(entropy, Some(password))
    val actualEd25519Sk = ed25519Initializer.fromEntropy(entropy, Some(password))
    val actualVrf25519Sk = vrfInitializer(new Ed25519VRF).fromEntropy(entropy, Some(password))
    val actualExtended25519Sk = extendedEd25519Initializer.fromEntropy(entropy, Some(password))

    implicit val outputsDecoder: Encoder[SpecOutputs] = (s: SpecOutputs) =>
      Json.obj(
        "curve25519" -> Hex.encode(s.curve25519.bytes.data).asJson,
        "ed25519"    -> Hex.encode(s.ed25519.bytes.data).asJson,
        "vrfEd25519" -> Hex.encode(s.vrfEd25519.bytes.data).asJson,
        "extendedEd25519" -> Hex
          .encode(
            s.extendedEd25519.leftKey.data ++ s.extendedEd25519.rightKey.data ++ s.extendedEd25519.chainCode.data
          )
          .asJson
      )

    println(SpecOutputs(actualCurve25519Sk, actualEd25519Sk, actualVrf25519Sk, actualExtended25519Sk).asJson)
  }

}
