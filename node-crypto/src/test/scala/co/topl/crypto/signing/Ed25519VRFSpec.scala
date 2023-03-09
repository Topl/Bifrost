package co.topl.crypto.signing

import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.utils.EntropySupport._
import co.topl.crypto.utils._
import io.circe.Decoder
import io.circe.HCursor
import io.circe.generic.semiauto.deriveDecoder
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scodec.bits.ByteVector

/**
 * Reference -https://github.com/Topl/reference_crypto/tree/main/specs/crypto/signing/VRF-Ed25519-Sha512-TAI
 *
 * Test vectors for Topl implementation of ECVRF-EDWARDS25519-SHA512-TAI ( https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-vrf-09).
 *
 * Test vector are adopted from the underlying Edwards25519 signing routine as given
 * here https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-vrf-09#appendix-A.3
 *
 * All values below are Hex encoded byte representations unless otherwise specified.
 */
class Ed25519VRFSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  property("with Ed25519VRF, signed message should be verifiable with appropriate public key") {
    forAll { (entropy1: Entropy, entropy2: Entropy, message1: Array[Byte], message2: Array[Byte]) =>
      whenever((entropy1 != entropy2) && !(message1 sameElements message2)) {
        val ed25519vrf = new Ed25519VRF
        val (sk1, vk1) = ed25519vrf.deriveKeyPairFromEntropy(entropy1, None)
        val (_, vk2) = ed25519vrf.deriveKeyPairFromEntropy(entropy2, None)
        val sig = ed25519vrf.sign(sk1, message1)

        ed25519vrf.verify(sig, message1, vk1) shouldBe true
        ed25519vrf.verify(sig, message1, vk2) shouldBe false
        ed25519vrf.verify(sig, message2, vk1) shouldBe false
      }
    }
  }

  property("with Ed25519VRF, keyPairs generated with the same seed should be the same") {
    forAll { seedByteVector: Entropy =>
      whenever(seedByteVector.value.length != 0) {
        val ed25519vrf = new Ed25519VRF
        val keyPair1 = ed25519vrf.deriveKeyPairFromEntropy(seedByteVector, None)
        val keyPair2 = ed25519vrf.deriveKeyPairFromEntropy(seedByteVector, None)

        ByteVector(keyPair1._1) shouldBe ByteVector(keyPair2._1)
        ByteVector(keyPair1._2) shouldBe ByteVector(keyPair2._2)
      }
    }
  }

  property("Topl specific seed generation mechanism should generate a fixed secret key given an entropy and password") {
    val e = Entropy(ByteVector.encodeUtf8("topl").toOption.get)
    val p = "topl"
    val specOutSK =
      ByteVector(Hex.decode("d8f0ad4d22ec1a143905af150e87c7f0dadd13749ef56fbd1bb380c37bc18cf8"))
    val specOutVK =
      ByteVector(Hex.decode("8ecfec14ce183dd6e747724993a9ae30328058fd85fa1e3c6f996b61bb164fa8"))

    val underTest = new Ed25519VRF
    val (sk, vk) = underTest.deriveKeyPairFromEntropy(e, Some(p))
    ByteVector(sk) shouldBe specOutSK
    ByteVector(vk) shouldBe specOutVK
  }

  VrfEd25519SpecHelper.testVectors.foreach { underTest =>
    property(s"${underTest.description}") {
      val ed25519vrf = new Ed25519VRF

      val vk = ed25519vrf.getVerificationKey(underTest.inputs.secretKey.toArray)
      val pi = ed25519vrf.sign(underTest.inputs.secretKey.toArray, underTest.inputs.message.toArray)

      ed25519vrf.verify(pi, underTest.inputs.message.toArray, vk) shouldBe true
      ed25519vrf.verify(pi, underTest.inputs.message.toArray, underTest.outputs.verificationKey.toArray) shouldBe true
      ed25519vrf.verify(underTest.outputs.pi.toArray, underTest.inputs.message.toArray, vk) shouldBe true
      ed25519vrf.verify(
        underTest.outputs.pi.toArray,
        underTest.inputs.message.toArray,
        underTest.outputs.verificationKey.toArray
      ) shouldBe true
      ByteVector(ed25519vrf.proofToHash(pi)) shouldBe underTest.outputs.beta
    }
  }

  object VrfEd25519SpecHelper {
    case class SpecInputs(secretKey: ByteVector, message: ByteVector)

    /**
     * @param beta length = 64
     */
    case class SpecOutputs(
      verificationKey: ByteVector,
      pi:              ByteVector,
      beta:            ByteVector
    )

    case class VrfEd25519TestVector(description: String, inputs: SpecInputs, outputs: SpecOutputs) extends TestVector

    implicit val inputsDecoder: Decoder[SpecInputs] = (c: HCursor) =>
      for {
        sk <- c
          .downField("secretKey")
          .as[String]
          .map(b => ByteVector(Hex.decode(b)))
        msg <- c.downField("message").as[String].map(b => ByteVector(Hex.decode(b)))
      } yield SpecInputs(sk, msg)

    implicit val outputsDecoder: Decoder[SpecOutputs] = (c: HCursor) =>
      for {
        vk <- c
          .downField("verificationKey")
          .as[String]
          .map(b => ByteVector(Hex.decode(b)))
        pi <- c
          .downField("pi")
          .as[String]
          .map(b => ByteVector(Hex.decode(b)))
        beta <- c
          .downField("beta")
          .as[String]
          .map(b => ByteVector(Hex.decode(b)))
      } yield SpecOutputs(vk, pi, beta)

    implicit val testVectorDecoder: Decoder[VrfEd25519TestVector] = deriveDecoder[VrfEd25519TestVector]
    val testVectors: List[VrfEd25519TestVector] = TestVector.read("signing/VrfEd25519.json")
  }
}
