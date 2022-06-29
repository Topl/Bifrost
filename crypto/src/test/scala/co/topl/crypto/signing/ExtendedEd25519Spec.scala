package co.topl.crypto.signing

import cats.implicits._
import co.topl.crypto.generation.{Bip32Index, Bip32Indexes}
import co.topl.crypto.generation.KeyInitializer.Instances.extendedEd25519Initializer
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
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import java.nio.charset.StandardCharsets

class ExtendedEd25519Spec
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with EitherValues {

  private val passwordOpt: Option[Password] = None
  implicit val extendedEd25519: ExtendedEd25519 = new ExtendedEd25519

  property("verify a signed message using the appropriate public key") {
    forAll { (entropy1: Entropy, entropy: Entropy, message1: Bytes, message2: Bytes) =>
      whenever(!(entropy1 == entropy) && !(message1 == message2)) {
        val extendedEd25519 = new ExtendedEd25519
        val (sk1, vk1) = extendedEd25519.deriveKeyPairFromEntropy(entropy1, passwordOpt)
        val (_, vk2) = extendedEd25519.deriveKeyPairFromEntropy(entropy, passwordOpt)
        val sig = extendedEd25519.sign(sk1, message1)

        extendedEd25519.verify(sig, message1, vk1) shouldBe true
        extendedEd25519.verify(sig, message1, vk2) shouldBe false
        extendedEd25519.verify(sig, message2, vk1) shouldBe false
      }
    }
  }

  property("generate identical keypairs given the same seed") {
    forAll { entropy: Entropy =>
      whenever(entropy.value.length != 0) {
        val extendedEd25519 = new ExtendedEd25519
        val keyPair1 = extendedEd25519.deriveKeyPairFromEntropy(entropy, passwordOpt)
        val keyPair2 = extendedEd25519.deriveKeyPairFromEntropy(entropy, passwordOpt)

        keyPair1._1 === keyPair2._1 shouldBe true
        keyPair1._2 === keyPair2._2 shouldBe true
      }
    }
  }

  property("should generate a specific secret key given a specific entropy and password") {
    val e = Entropy(Bytes("topl".getBytes(StandardCharsets.UTF_8)))
    val p = "topl"
    val specOutSK =
      SecretKeys.ExtendedEd25519(
        "d8f0ad4d22ec1a143905af150e87c7f0dadd13749ef56fbd1bb380c37bc18c58".unsafeStrictBytes,
        "a900381746984a637dd3fa454419a6d560d14d4142921895575f406c9ad8d92d".unsafeStrictBytes,
        "cd07b700697afb30785ac4ab0ca690fd87223a12a927b4209ecf2da727ecd039".unsafeStrictBytes
      )
    val specOutVK =
      VerificationKeys.ExtendedEd25519(
        VerificationKeys.Ed25519("e684c4a4442a9e256b18460b74e0bdcd1c4c9a7f4c504e8555670f69290f142d".unsafeStrictBytes),
        "cd07b700697afb30785ac4ab0ca690fd87223a12a927b4209ecf2da727ecd039".unsafeStrictBytes
      )

    val underTest = new ExtendedEd25519
    val (sk, vk) = underTest.deriveKeyPairFromEntropy(e, Some(p))
    sk shouldBe specOutSK
    vk shouldBe specOutVK
  }

  ExtendedEd25519SigningSpecHelper.testVectors.foreach { underTest =>
    property(s"${underTest.description}") {

      val vk = extendedEd25519.getVerificationKey(underTest.inputs.secretKey)
      val sig = extendedEd25519.sign(underTest.inputs.secretKey, underTest.inputs.message)

      extendedEd25519.verify(sig, underTest.inputs.message, vk) shouldBe true
      extendedEd25519.verify(sig, underTest.inputs.message, underTest.outputs.verificationKey) shouldBe true
      extendedEd25519.verify(underTest.outputs.signature, underTest.inputs.message, vk) shouldBe true
      extendedEd25519.verify(
        underTest.outputs.signature,
        underTest.inputs.message,
        underTest.outputs.verificationKey
      ) shouldBe true
    }
  }

  ExtendedEd25519CKDSpecHelper.testVectors.foreach { underTest =>
    property(s"${underTest.description}") {

      val derivedChild_xsk =
        underTest.inputs.path.foldLeft(underTest.inputs.rootSecretKey)((xsk, ind) =>
          extendedEd25519.deriveSecret(xsk, ind)
        )
      val fromDerivedChildSk_xvk = extendedEd25519.getVerificationKey(derivedChild_xsk)
      val derivedChild_xvk = underTest.inputs.rootVerificationKey.map { vk =>
        underTest.inputs.path.foldLeft(vk) {
          case (xvk, ind: Bip32Indexes.SoftIndex) =>
            extendedEd25519.deriveVerification(xvk, ind)
          case _ => throw new Exception("received hardened index when soft index was expected")
        }
      }

      derivedChild_xsk shouldBe underTest.outputs.childSecretKey
      fromDerivedChildSk_xvk shouldBe underTest.outputs.childVerificationKey

      derivedChild_xvk.foreach { input_xvk =>
        input_xvk shouldBe underTest.outputs.childVerificationKey
        input_xvk shouldBe fromDerivedChildSk_xvk
      }
    }
  }

  object ExtendedEd25519SigningSpecHelper {
    case class SpecInputs(secretKey: SecretKeys.ExtendedEd25519, message: Bytes)

    case class SpecOutputs(verificationKey: VerificationKeys.ExtendedEd25519, signature: Proofs.Knowledge.Ed25519)

    case class ExtendedEd25519SignTestVector(description: String, inputs: SpecInputs, outputs: SpecOutputs)
        extends TestVector

    implicit def inputsDecoder(implicit extendedEd25519: ExtendedEd25519): Decoder[SpecInputs] = (c: HCursor) =>
      for {
        sk <- c
          .get[String]("secretKey")
          .map(extendedEd25519Initializer.fromBase16String(_).value)
        msg <- c.downField("message").as[String].map(b => Bytes(Hex.decode(b)))
      } yield SpecInputs(sk, msg)

    implicit def outputsDecoder(implicit extendedEd25519: ExtendedEd25519): Decoder[SpecOutputs] = (c: HCursor) =>
      for {
        vk <- c
          .get[String]("verificationKey")
          .map { base16String =>
            val bytes = Hex.decode(base16String)
            VerificationKeys.ExtendedEd25519(
              VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(bytes.slice(0, 32)))),
              Sized.strictUnsafe(Bytes(bytes.slice(32, 64)))
            )
          }
        sig <- c
          .get[String]("signature")
          .map(b => Proofs.Knowledge.Ed25519(Sized.strictUnsafe(Bytes(Hex.decode(b)))))
      } yield SpecOutputs(vk, sig)

    implicit def testVectorDecoder: Decoder[ExtendedEd25519SignTestVector] =
      deriveDecoder[ExtendedEd25519SignTestVector]
    val testVectors: List[ExtendedEd25519SignTestVector] = TestVector.read("signing/ExtendedEd25519.json")
  }

  object ExtendedEd25519CKDSpecHelper {

    case class SpecInputs(
      rootSecretKey:       SecretKeys.ExtendedEd25519,
      rootVerificationKey: Option[VerificationKeys.ExtendedEd25519],
      path:                Vector[Bip32Index]
    )

    case class SpecOutputs(
      childSecretKey:       SecretKeys.ExtendedEd25519,
      childVerificationKey: VerificationKeys.ExtendedEd25519
    )

    case class ExtendedEd25519CKDTestVector(description: String, inputs: SpecInputs, outputs: SpecOutputs)
        extends TestVector

    implicit def inputsDecoder(implicit extendedEd25519: ExtendedEd25519): Decoder[SpecInputs] = (c: HCursor) =>
      for {
        rootSk <- c
          .get[String]("rootSecretKey")
          .map(extendedEd25519Initializer.fromBase16String(_).value)
        rootVkString <- c.get[Option[String]]("rootVerificationKey")
        rootVkOpt = rootVkString.map { hexString =>
          val rootVkBytes = Hex.decode(hexString)
          VerificationKeys.ExtendedEd25519(
            VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(rootVkBytes.slice(0, 32)))),
            Sized.strictUnsafe(Bytes(rootVkBytes.slice(32, 64)))
          )
        }
        path <- c
          .get[Vector[(String, Long)]]("path")
          .map(_.map {
            case ("soft", index) => Bip32Indexes.SoftIndex(index)
            case ("hard", index) => Bip32Indexes.HardenedIndex(index)
            case _               => throw new Exception("how not to do?")
          })
      } yield SpecInputs(rootSk, rootVkOpt, path)

    implicit def outputsDecoder(implicit extendedEd25519: ExtendedEd25519): Decoder[SpecOutputs] = (c: HCursor) =>
      for {
        childSk <- c
          .get[String]("childSecretKey")
          .map(extendedEd25519Initializer.fromBase16String(_).value)
        childVk <- c
          .get[String]("childVerificationKey")
          .map { base16String =>
            val bytes = Hex.decode(base16String)
            VerificationKeys.ExtendedEd25519(
              VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(bytes.slice(0, 32)))),
              Sized.strictUnsafe(Bytes(bytes.slice(32, 64)))
            )
          }
      } yield SpecOutputs(childSk, childVk)

    implicit def testVectorDecoder: Decoder[ExtendedEd25519CKDTestVector] =
      deriveDecoder[ExtendedEd25519CKDTestVector]

    val testVectors: List[ExtendedEd25519CKDTestVector] =
      TestVector.read("generation/Bip32-Ed25519_ChildKeyDerivation.json")
  }
}
