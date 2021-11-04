package co.topl.crypto.signing

import co.topl.crypto.utils.Hex
import co.topl.crypto.utils.Hex.implicits._
import co.topl.models.ModelGenerators.arbitraryBytes
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Sized
import co.topl.models.{Bytes, Proofs, SecretKeys, VerificationKeys}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

/**
 * Test vectors available at https://github.com/Topl/reference_crypto/tree/main/specs/crypto/signing/Curve25519-Axolotl
 */
class Curve25519AxolotlSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  property("signed message should be verifiable with appropriate public key") {
    forAll { (seed1: Bytes, seed2: Bytes, message1: Bytes, message2: Bytes) =>
      whenever(!(seed1 == seed2) && !(message1 == message2)) {
        val curve25519 = new Curve25519

        val (sk1, vk1) = curve25519.createKeyPair(seed1)
        val (_, vk2) = curve25519.createKeyPair(seed2)

        val sig = curve25519.sign(sk1, message1)

        curve25519.verify(sig, message1, vk1) shouldBe true
        curve25519.verify(sig, message1, vk2) should not be true
        curve25519.verify(sig, message2, vk1) should not be true
      }
    }
  }
  property("with Curve25519, keyPairs generated with the same seed should be the same") {
    forAll { seedBytes: Bytes =>
      whenever(seedBytes.nonEmpty) {
        val curve25519 = new Curve25519

        val (sk1, vk1) = curve25519.createKeyPair(seedBytes)
        val (sk2, vk2) = curve25519.createKeyPair(seedBytes)

        sk1 === sk2 shouldBe true
        vk1 === vk2 shouldBe true
      }
    }
  }
  property("test vector - 1") {
    val curve25519 = new Curve25519
    val specInSk = SecretKeys.Curve25519(
      "184F0E9851971998E732078544C96B36C3D01CEDF7CAA332359D6F1D83567054".unsafeStrictBytes
    )
    val specInMsg = Bytes(Hex.decode(""))

    val vk = curve25519.getVerificationKey(specInSk)
    val sig = curve25519.sign(specInSk, specInMsg)

    val specOutVk = VerificationKeys.Curve25519(
      "4652486EBC271520D844E5BDDA9AC243C05DCBE7BC9B93807073A32177A6F73D".unsafeStrictBytes
    )
    val specOutSig = Proofs.Signature.Curve25519(
      Sized.strictUnsafe(
        Bytes(
          Hex.decode(
            "AD7B2C434DE419712A55AF65D485DA4F673076D4FDBFF4730A20AA8DC1F0C05A" +
            "D757F6B50674CFB622131377F29C646DF60C7148E6B8AF33850276F98D31DA0A"
          )
        )
      )
    )

    curve25519.verify(sig, specInMsg, vk) shouldBe true
    curve25519.verify(sig, specInMsg, specOutVk) shouldBe true
    curve25519.verify(specOutSig, specInMsg, vk) shouldBe true
    curve25519.verify(specOutSig, specInMsg, specOutVk) shouldBe true
  }
  property("test vector - 2, one byte message length") {
    val curve25519 = new Curve25519
    val specInSk = SecretKeys.Curve25519(
      "60303AE22B998861BCE3B28F33EEC1BE758A213C86C93C076DBE9F558C11C752".unsafeStrictBytes
    )
    val specInMsg = Bytes(Hex.decode("72"))

    val vk = curve25519.getVerificationKey(specInSk)
    val sig = curve25519.sign(specInSk, specInMsg)

    val specOutVk = VerificationKeys.Curve25519(
      "FFBC7BA2E4C43BE03F8A7F020D0651F582AD1901C254EEBB4EC2ECB73148E50D".unsafeStrictBytes
    )
    val specOutSig = Proofs.Signature.Curve25519(
      "E7D628E6A25AFF86A7A81CF60B40D0C8BCA038A75CB6BEF34B384E5D098C1F291FD7068CDBF001CAF0EB82810B0A13F0B2806E02124E416B671E51241DD4EB0F".unsafeStrictBytes
    )

    curve25519.verify(sig, specInMsg, vk) shouldBe true
    curve25519.verify(sig, specInMsg, specOutVk) shouldBe true
    curve25519.verify(specOutSig, specInMsg, vk) shouldBe true
    curve25519.verify(specOutSig, specInMsg, specOutVk) shouldBe true
  }
  property("test vector - 3, two bytes message length") {
    val curve25519 = new Curve25519
    val specInSk = SecretKeys.Curve25519(
      "F861A03AF4F77D870FC21E05E7E80678095C92D808CFB3B5C279EE04C74ACA53".unsafeStrictBytes
    )
    val specInMsg = Bytes(Hex.decode("af82"))

    val vk = curve25519.getVerificationKey(specInSk)
    val sig = curve25519.sign(specInSk, specInMsg)

    val specOutVk = VerificationKeys.Curve25519(
      "59DF714EAD8FB10B68E31153AD01994117652CB3C960C6E32C57E7DEC28A5846".unsafeStrictBytes
    )
    val specOutSig = Proofs.Signature.Curve25519(
      "12ED7C9B5D757C23809FF620CF0B48CF054F6FC60B8B45B50C49F78C80332D1B4F1DEF16F3270E75686CF12F661CF777EBDCFF2977078D5EEC32F0EB39D11802".unsafeStrictBytes
    )

    curve25519.verify(sig, specInMsg, vk) shouldBe true
    curve25519.verify(sig, specInMsg, specOutVk) shouldBe true
    curve25519.verify(specOutSig, specInMsg, vk) shouldBe true
    curve25519.verify(specOutSig, specInMsg, specOutVk) shouldBe true
  }
  property("test vector - 4, 1023 bytes message length") {
    val curve25519 = new Curve25519
    val specInSk = SecretKeys.Curve25519(
      "30929AC0656504EF9782D0164A3603C09AA3FDCB07973948D03472A571CAB769".unsafeStrictBytes
    )
    val specInMsg = Bytes(
      Hex.decode(
        "08b8b2b733424243760fe426a4b54908632110a66c2f6591eabd3345e3e4eb98fa6e264bf09efe12ee50" +
        "f8f54e9f77b1e355f6c50544e23fb1433ddf73be84d879de7c0046dc4996d9e773f4bc9efe5738829adb26c81b37c93a1b270b20329d65" +
        "8675fc6ea534e0810a4432826bf58c941efb65d57a338bbd2e26640f89ffbc1a858efcb8550ee3a5e1998bd177e93a7363c344fe6b199e" +
        "e5d02e82d522c4feba15452f80288a821a579116ec6dad2b3b310da903401aa62100ab5d1a36553e06203b33890cc9b832f79ef80560cc" +
        "b9a39ce767967ed628c6ad573cb116dbefefd75499da96bd68a8a97b928a8bbc103b6621fcde2beca1231d206be6cd9ec7aff6f6c94fcd" +
        "7204ed3455c68c83f4a41da4af2b74ef5c53f1d8ac70bdcb7ed185ce81bd84359d44254d95629e9855a94a7c1958d1f8ada5d0532ed8a5" +
        "aa3fb2d17ba70eb6248e594e1a2297acbbb39d502f1a8c6eb6f1ce22b3de1a1f40cc24554119a831a9aad6079cad88425de6bde1a9187e" +
        "bb6092cf67bf2b13fd65f27088d78b7e883c8759d2c4f5c65adb7553878ad575f9fad878e80a0c9ba63bcbcc2732e69485bbc9c90bfbd6" +
        "2481d9089beccf80cfe2df16a2cf65bd92dd597b0707e0917af48bbb75fed413d238f5555a7a569d80c3414a8d0859dc65a46128bab27a" +
        "f87a71314f318c782b23ebfe808b82b0ce26401d2e22f04d83d1255dc51addd3b75a2b1ae0784504df543af8969be3ea7082ff7fc9888c" +
        "144da2af58429ec96031dbcad3dad9af0dcbaaaf268cb8fcffead94f3c7ca495e056a9b47acdb751fb73e666c6c655ade8297297d07ad1" +
        "ba5e43f1bca32301651339e22904cc8c42f58c30c04aafdb038dda0847dd988dcda6f3bfd15c4b4c4525004aa06eeff8ca61783aacec57" +
        "fb3d1f92b0fe2fd1a85f6724517b65e614ad6808d6f6ee34dff7310fdc82aebfd904b01e1dc54b2927094b2db68d6f903b68401adebf5a" +
        "7e08d78ff4ef5d63653a65040cf9bfd4aca7984a74d37145986780fc0b16ac451649de6188a7dbdf191f64b5fc5e2ab47b57f7f7276cd4" +
        "19c17a3ca8e1b939ae49e488acba6b965610b5480109c8b17b80e1b7b750dfc7598d5d5011fd2dcc5600a32ef5b52a1ecc820e308aa342" +
        "721aac0943bf6686b64b2579376504ccc493d97e6aed3fb0f9cd71a43dd497f01f17c0e2cb3797aa2a2f256656168e6c496afc5fb93246" +
        "f6b1116398a346f1a641f3b041e989f7914f90cc2c7fff357876e506b50d334ba77c225bc307ba537152f3f1610e4eafe595f6d9d90d11" +
        "faa933a15ef1369546868a7f3a45a96768d40fd9d03412c091c6315cf4fde7cb68606937380db2eaaa707b4c4185c32eddcdd306705e4d" +
        "c1ffc872eeee475a64dfac86aba41c0618983f8741c5ef68d3a101e8a3b8cac60c905c15fc910840b94c00a0b9d0"
      )
    )

    val vk = curve25519.getVerificationKey(specInSk)
    val sig = curve25519.sign(specInSk, specInMsg)

    val specOutVk = VerificationKeys.Curve25519(
      "05C54041990DEF35C6B163F1B6DBE5194BC4139C7D3BA8F1FCF28DE3A8B07357".unsafeStrictBytes
    )
    val specOutSig = Proofs.Signature.Curve25519(
      "AA9D3EF915F416A85717A00B69C4CB01514AEB9D743259974F36B9EEB429BE4039C0491C68EE3987F493BFB7D54039BA2D03DD8CBF83A1D978E0B18C4C363680".unsafeStrictBytes
    )

    curve25519.verify(sig, specInMsg, vk) shouldBe true
    curve25519.verify(sig, specInMsg, specOutVk) shouldBe true
    curve25519.verify(specOutSig, specInMsg, vk) shouldBe true
    curve25519.verify(specOutSig, specInMsg, specOutVk) shouldBe true
  }
  property("test vector - 5, abc hashed by SHA512 as message") {
    val curve25519 = new Curve25519
    val specInSk = SecretKeys.Curve25519(
      "C8A312FBBCF8FF3213B917D4232BCA39AAE7740338791114072F07FF3692CA72".unsafeStrictBytes
    )
    val specInMsg = Bytes(
      Hex.decode(
        "DDAF35A193617ABACC417349AE20413112E6FA4E89A97EA20A9EEEE64B55D39A2192992A274FC1A836BA3C23A3FEEBBD454D4423643CE80E2A9AC94FA54CA49F"
      )
    )

    val vk = curve25519.getVerificationKey(specInSk)
    val sig = curve25519.sign(specInSk, specInMsg)

    val specOutVk = VerificationKeys.Curve25519(
      "5EFBB8396F73F2663403B242347E8B72FBA96DD1E92DBA71A909076643631752".unsafeStrictBytes
    )
    val specOutSig = Proofs.Signature.Curve25519(
      "DC8566EF933EEE7527E7150C5464CAA67F244BF6DCE052821B5A7424892D28798C6674E2B6671E3AB3E73B0FA8F2BC960714FF05F48DA91CD720FE7140875E87".unsafeStrictBytes
    )

    curve25519.verify(sig, specInMsg, vk) shouldBe true
    curve25519.verify(sig, specInMsg, specOutVk) shouldBe true
    curve25519.verify(specOutSig, specInMsg, vk) shouldBe true
    curve25519.verify(specOutSig, specInMsg, specOutVk) shouldBe true
  }
}
