package co.topl.crypto.signing

import cats.implicits._
import co.topl.crypto.mnemonic.Entropy
import co.topl.crypto.mnemonic.EntropySupport._
import co.topl.crypto.utils.Hex
import co.topl.crypto.utils.Hex.implicits._
import co.topl.models.ModelGenerators.arbitraryBytes
import co.topl.models.utility.{Lengths, Sized}
import co.topl.models.{Bytes, Proofs, SecretKeys, VerificationKeys}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.nio.charset.StandardCharsets

class Ed25519VRFSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  property("with Ed25519VRF, signed message should be verifiable with appropriate public key") {
    forAll { (entropy1: Entropy, entropy2: Entropy, message1: Bytes, message2: Bytes) =>
      whenever((entropy1 =!= entropy2) && !(message1 == message2)) {
        val ed25519vrf = new Ed25519VRF
        val (sk1, vk1) = ed25519vrf.createKeyPair(entropy1, None)
        val (_, vk2) = ed25519vrf.createKeyPair(entropy2, None)
        val sig = ed25519vrf.sign(sk1, message1)

        ed25519vrf.verify(sig, message1, vk1) shouldBe true
        ed25519vrf.verify(sig, message1, vk2) shouldBe false
        ed25519vrf.verify(sig, message2, vk1) shouldBe false
      }
    }
  }
  property("with Ed25519VRF, keyPairs generated with the same seed should be the same") {
    forAll { seedBytes: Entropy =>
      whenever(seedBytes.value.length != 0) {
        val ed25519vrf = new Ed25519VRF
        val keyPair1 = ed25519vrf.createKeyPair(seedBytes, None)
        val keyPair2 = ed25519vrf.createKeyPair(seedBytes, None)

        keyPair1._1 === keyPair2._1 shouldBe true
        keyPair1._2 === keyPair2._2 shouldBe true
      }
    }
  }
  property("test vectors from https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-vrf-09#appendix-A.3 - test 1") {
    val ed25519vrf = new Ed25519VRF
    val specIn_sk =
      SecretKeys.VrfEd25519("9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60".unsafeStrictBytes)
    val specIn_msg = Bytes(Hex.decode(""))

    val specOut_vk = VerificationKeys.VrfEd25519(
      "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a".unsafeStrictBytes
    )
    val specOut_pi = Proofs.Signature.VrfEd25519(
      "8657106690b5526245a92b003bb079ccd1a92130477671f6fc01ad16f26f723f5e8bd1839b414219e8626d393787a192241fc442e6569e96c462f62b8079b9ed83ff2ee21c90c7c398802fdeebea4001".unsafeStrictBytes
    )
    val specOut_beta: Sized.Strict[Bytes, Lengths.`64`.type] =
      "90cf1df3b703cce59e2a35b925d411164068269d7b2d29f3301c03dd757876ff66b71dda49d2de59d03450451af026798e8f81cd2e333de5cdf4f3e140fdd8ae".unsafeStrictBytes

    val pi = ed25519vrf.sign(specIn_sk, specIn_msg)
    val beta = ed25519vrf.proofToHash(pi)

    pi shouldBe specOut_pi
    beta shouldBe specOut_beta
    ed25519vrf.verify(pi, specIn_msg, specOut_vk) shouldBe true
  }
  property("test vectors from https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-vrf-09#appendix-A.3 - test 2") {
    val ed25519vrf = new Ed25519VRF
    val specIn_sk =
      SecretKeys.VrfEd25519("4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb".unsafeStrictBytes)
    val specIn_msg = Bytes(Hex.decode("72"))

    val specOut_vk = VerificationKeys.VrfEd25519(
      "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c".unsafeStrictBytes
    )
    val specOut_pi = Proofs.Signature.VrfEd25519(
      "f3141cd382dc42909d19ec5110469e4feae18300e94f304590abdced48aed593f7eaf3eb2f1a968cba3f6e23b386aeeaab7b1ea44a256e811892e13eeae7c9f6ea8992557453eac11c4d5476b1f35a08".unsafeStrictBytes
    )
    val specOut_beta: Sized.Strict[Bytes, Lengths.`64`.type] =
      "eb4440665d3891d668e7e0fcaf587f1b4bd7fbfe99d0eb2211ccec90496310eb5e33821bc613efb94db5e5b54c70a848a0bef4553a41befc57663b56373a5031".unsafeStrictBytes

    val pi = ed25519vrf.sign(specIn_sk, specIn_msg)

    ed25519vrf.verify(pi, specIn_msg, specOut_vk) shouldBe true
    ed25519vrf.verify(specOut_pi, specIn_msg, specOut_vk) shouldBe true
    ed25519vrf.proofToHash(pi) shouldBe specOut_beta
  }
  property("test vectors from https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-vrf-09#appendix-A.3 - test 3") {
    val ed25519vrf = new Ed25519VRF
    val specIn_sk =
      SecretKeys.VrfEd25519("c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7".unsafeStrictBytes)
    val specIn_msg = Bytes(Hex.decode("af82"))

    val specOut_vk = VerificationKeys.VrfEd25519(
      "fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025".unsafeStrictBytes
    )
    val specOut_pi = Proofs.Signature.VrfEd25519(
      "9bc0f79119cc5604bf02d23b4caede71393cedfbb191434dd016d30177ccbf80e29dc513c01c3a980e0e545bcd848222d08a6c3e3665ff5a4cab13a643bef812e284c6b2ee063a2cb4f456794723ad0a".unsafeStrictBytes
    )
    val specOut_beta: Sized.Strict[Bytes, Lengths.`64`.type] =
      "645427e5d00c62a23fb703732fa5d892940935942101e456ecca7bb217c61c452118fec1219202a0edcf038bb6373241578be7217ba85a2687f7a0310b2df19f".unsafeStrictBytes

    val pi = ed25519vrf.sign(specIn_sk, specIn_msg)

    ed25519vrf.verify(pi, specIn_msg, specOut_vk) shouldBe true
    ed25519vrf.verify(specOut_pi, specIn_msg, specOut_vk) shouldBe true
    ed25519vrf.proofToHash(pi) shouldBe specOut_beta
  }

  property(
    "Topl seed generation mechanism should generate a specific secret key given a specific entropy and password"
  ) {
    val e = Entropy("topl".getBytes(StandardCharsets.UTF_8))
    val p = "topl"
    val specOutSK =
      SecretKeys.VrfEd25519("911ccb6f6e2a37ff24cac9a570ca3b8e342e20678e30d783cd3226b22b5883e2".unsafeStrictBytes)
    val specOutVK =
      VerificationKeys.VrfEd25519("c6c07e313d0c0cb8fff25e25d660580fadbff7b1afb9cdfe9ec62c9169a92014".unsafeStrictBytes)

    val underTest = new Ed25519VRF
    val (sk, vk) = underTest.createKeyPair(e, Some(p))
    sk shouldBe specOutSK
    vk shouldBe specOutVK
  }
}
