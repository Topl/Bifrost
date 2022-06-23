package co.topl.crypto.signing

import cats.implicits._
import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.utils.EntropySupport._
import co.topl.crypto.utils.Hex.implicits._
import co.topl.crypto.utils.{Hex, TestVector}
import co.topl.models.ModelGenerators.arbitraryBytes
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Sized
import co.topl.models.{Bytes, Proofs, SecretKeys, VerificationKeys}
import io.circe.generic.semiauto.deriveDecoder
import io.circe.{Decoder, HCursor}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.nio.charset.StandardCharsets

/**
 * Test vectors available at https://github.com/Topl/reference_crypto/tree/main/specs/crypto/signing/Curve25519-Axolotl
 */
class Curve25519AxolotlSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  property("signed message should be verifiable with appropriate public key") {
    forAll { (entropy1: Entropy, entropy2: Entropy, message1: Bytes, message2: Bytes) =>
      whenever((entropy1 =!= entropy2) && !(message1 == message2)) {
        val curve25519 = new Curve25519

        val (sk1, vk1) = curve25519.deriveKeyPairFromEntropy(entropy1, None)
        val (_, vk2) = curve25519.deriveKeyPairFromEntropy(entropy2, None)

        val sig = curve25519.sign(sk1, message1)

        curve25519.verify(sig, message1, vk1) shouldBe true
        curve25519.verify(sig, message1, vk2) should not be true
        curve25519.verify(sig, message2, vk1) should not be true
      }
    }
  }

  property("with Curve25519, keyPairs generated with the same seed should be the same") {
    forAll { entropy: Entropy =>
      whenever(entropy.value.nonEmpty) {
        val curve25519 = new Curve25519

        val (sk1, vk1) = curve25519.deriveKeyPairFromEntropy(entropy, None)
        val (sk2, vk2) = curve25519.deriveKeyPairFromEntropy(entropy, None)

        sk1 === sk2 shouldBe true
        vk1 === vk2 shouldBe true
      }
    }
  }

  property(
    "Topl specific seed generation mechanism should generate a fixed secret key given a fixed entropy and password"
  ) {
    val e = Entropy("topl".getBytes(StandardCharsets.UTF_8))
    val p = "topl"
    val specOutSK =
      SecretKeys.Curve25519("d8f0ad4d22ec1a143905af150e87c7f0dadd13749ef56fbd1bb380c37bc18c78".unsafeStrictBytes)
    val specOutVK =
      VerificationKeys.Curve25519("6f7ba13496617a75044a201608d7b96ee56f43ccbb2acd21aa5ccf9ab2bbc544".unsafeStrictBytes)

    val underTest = new Curve25519
    val (sk, vk) = underTest.deriveKeyPairFromEntropy(e, Some(p))
    sk shouldBe specOutSK
    vk shouldBe specOutVK
  }

  Curve25519AxolotlSpecHelper.testVectors.foreach { underTest =>
    property(s"${underTest.description}") {
      val curve25519 = new Curve25519

      val vk = curve25519.getVerificationKey(underTest.inputs.secretKey)
      val sig = curve25519.sign(underTest.inputs.secretKey, underTest.inputs.message)

      curve25519.verify(sig, underTest.inputs.message, vk) shouldBe true
      curve25519.verify(sig, underTest.inputs.message, underTest.outputs.verificationKey) shouldBe true
      curve25519.verify(underTest.outputs.signature, underTest.inputs.message, vk) shouldBe true
      curve25519.verify(
        underTest.outputs.signature,
        underTest.inputs.message,
        underTest.outputs.verificationKey
      ) shouldBe true
    }
  }

  object Curve25519AxolotlSpecHelper {
    case class SpecInputs(secretKey: SecretKeys.Curve25519, message: Bytes)
    case class SpecOutputs(verificationKey: VerificationKeys.Curve25519, signature: Proofs.Knowledge.Curve25519)

    case class Curve25519AxolotlTestVector(description: String, inputs: SpecInputs, outputs: SpecOutputs)
        extends TestVector

    implicit val inputsDecoder: Decoder[SpecInputs] = (c: HCursor) =>
      for {
        sk <- c
          .downField("secretKey")
          .as[String]
          .map(b => SecretKeys.Curve25519(Sized.strictUnsafe(Bytes(Hex.decode(b)))))
        msg <- c.downField("message").as[String].map(b => Bytes(Hex.decode(b)))
      } yield SpecInputs(sk, msg)

    implicit val outputsDecoder: Decoder[SpecOutputs] = (c: HCursor) =>
      for {
        vk <- c
          .downField("verificationKey")
          .as[String]
          .map(b => VerificationKeys.Curve25519(Sized.strictUnsafe(Bytes(Hex.decode(b)))))
        sig <- c
          .downField("signature")
          .as[String]
          .map(b => Proofs.Knowledge.Curve25519(Sized.strictUnsafe(Bytes(Hex.decode(b)))))

        // Hex.hexStringToStrictBytes[Proofs.Knowledge.Curve25519.Length](b)))
      } yield SpecOutputs(vk, sig)

    implicit val testVectorDecoder: Decoder[Curve25519AxolotlTestVector] = deriveDecoder[Curve25519AxolotlTestVector]
    val testVectors: List[Curve25519AxolotlTestVector] = TestVector.read("Curve25519Axolotl.json")
  }
}
