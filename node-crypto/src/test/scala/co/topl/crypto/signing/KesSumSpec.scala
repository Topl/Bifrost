package co.topl.crypto.signing

import co.topl.crypto.models._
import co.topl.crypto.utils.NodeCryptoGenerators._
import co.topl.crypto.utils.Hex.implicits._
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class KesSumSpec extends AnyFlatSpec with ScalaCheckPropertyChecks with ScalaCheckDrivenPropertyChecks with Matchers {

  "KesSum" should "verify a message signed with the appropriate public key" in {
    forAll(
      genRandomlySizedByteArray,
      genRandomlySizedByteArray,
      genRandomlySizedByteArray,
      genRandomlySizedByteArray,
      Gen.choose(1, 12)
    ) { (seed1, seed2, message1, message2, height: Int) =>
      whenever(!(seed1 sameElements seed2) && !(message1 sameElements message2)) {
        val kesSum = new KesSum
        val (sk1, vk1) = kesSum.createKeyPair(seed1, height, 0)
        val (_, vk2) = kesSum.createKeyPair(seed2, height, 0)
        val sig = kesSum.sign(sk1, message1)

        kesSum.verify(sig, message1, vk1) shouldBe true
        kesSum.verify(sig, message1, vk2) shouldBe false
        kesSum.verify(sig, message2, vk1) shouldBe false
      }
    }
  }

  it should "generate identical keypairs given the same seed" in {
    forAll(genByteArrayWithBoundedSize(0, 1024), Gen.choose(1, 12)) { (seedBytes, height: Int) =>
      val kesSum = new KesSum
      val (_, vk1) = kesSum.createKeyPair(seedBytes, height, 0)
      val (_, vk2) = kesSum.createKeyPair(seedBytes, height, 0)

      vk1 shouldBe vk2
    }
  }

  it should "Test Vector - 1.0: Generate and verify a specified sum composition signature at t = 0 using a provided seed, message, and height" in {
    val kesSum = new KesSum()
    val specIn_seed = "5b74fae39b7a367da736490fa4a2bac992d011bcfb1d39b4dfdb4cf3a6dd1def".hexStringToBytes.toArray
    val specIn_height = 1
    val specIn_time = 0
    val specIn_msg = "6c616e677561676520736861726573206120636f6d6d6f6e20746f6e677565".hexStringToBytes.toArray

    val specOut_vk =
      VerificationKeyKesSum(
        "4162a383fd371823120a8bb8573dcb91d4b7e95e946598d202330f7cb0571a49".hexStringToBytes.toArray,
        specIn_time
      )
    val specOut_sig_0 = SignatureKesSum(
      "210368f7c37b1e3c04b08dce0f28264af902388cfbc17b98e739adcf28b4edc5".hexStringToBytes.toArray,
      "3e2f86db64a383c144063c8a17a376e248e03a3e36e46607a03412f2f4d1b702f679e17368a58bb9d2f95a6eed27d6143663b116e4039fb05cb75f4bf9b1e10a".hexStringToBytes.toArray,
      Vector(
        "6d44789d918310507d283b48c57b96a94f68b2fd13969df8d1ce1356a0726574".hexStringToBytes.toArray
      )
    )

    val (sk, vk) = kesSum.createKeyPair(specIn_seed, specIn_height, 0)
    val sig_0 = kesSum.sign(sk, specIn_msg)
    vk shouldBe specOut_vk
    sig_0 shouldBe specOut_sig_0
    kesSum.verify(sig_0, specIn_msg, vk) shouldBe true
  }

  it should "Test Vector - 1.1 : Generate and verify a specified sum composition signature at t = 1 using a provided seed, message, and height" in {
    val kesSum = new KesSum()
    val specIn_seed = "5b74fae39b7a367da736490fa4a2bac992d011bcfb1d39b4dfdb4cf3a6dd1def".hexStringToBytes.toArray
    val specIn_height = 1
    val specIn_time = 0
    val specIn_msg = "6c616e677561676520736861726573206120636f6d6d6f6e20746f6e677565".hexStringToBytes.toArray

    val specOut_vk =
      VerificationKeyKesSum(
        "4162a383fd371823120a8bb8573dcb91d4b7e95e946598d202330f7cb0571a49".hexStringToBytes.toArray,
        specIn_time
      )
    val specOut_sig_1 = SignatureKesSum(
      "46d553fcac44083dd37ce353ad683b9cc3c4867e49cc5ebc778a2b64f80f0510".hexStringToBytes.toArray,
      "32d9610a6a6486a45b1f0328997dc20447f245378641799ddbe35a4fec20296e53b31fc63624a55bc80ae8892ff7b05c4089dc0253c98915db1c60c2e5e1d701".hexStringToBytes.toArray,
      Vector(
        "6d35211032dfede271b1c311f84b3dabfaf0a664c2c6a16eb7b6a3b3130ebf35".hexStringToBytes.toArray
      )
    )

    val (sk, vk) = kesSum.createKeyPair(specIn_seed, specIn_height, 0)
    val sk_1 = kesSum.update(sk, 1)
    val sig_1 = kesSum.sign(sk_1, specIn_msg)
    val vk_1 = kesSum.getVerificationKey(sk_1)
    vk shouldBe specOut_vk
    sig_1 shouldBe specOut_sig_1
    kesSum.verify(sig_1, specIn_msg, vk_1) shouldBe true
  }

  it should "Test Vector - 2 : Generate and verify a specified sum composition signature at t = [0, 1, 2, 3] using a provided seed, message, and height" in {
    val kesSum = new KesSum()
    val specIn_seed = "cd6fbfd1305556ca26b98077c7b1b0df79559c09f693fe0fc920f9f53fb0959f".hexStringToBytes.toArray
    val specIn_height = 2
    val specIn_time = 0
    val specIn_msg = "687564646c6564206d6173736573207769746820736f636b73".hexStringToBytes.toArray

    val specOut_vk =
      VerificationKeyKesSum(
        "15482211cce0a8c90a564ec64632b1eeea40c75b4575851303690afcece729e2".hexStringToBytes.toArray,
        specIn_time
      )
    val specOut_sig_0 = SignatureKesSum(
      "7b3060f42759972ae50b70521f9f920587ba1aaa9434fa4e059da40f4aa53f94".hexStringToBytes.toArray,
      (
        "5ba965947324fea9e78df19f7a2859f58aa2440ee2afad4c66e64f23ddfaafedcd27d7b8fd0db75c83caea394f270720407bdb0f4203a4d4d49f2f84b4f3a10b".hexStringToBytes.toArray
      ),
      Vector(
        "5a856ffe0c9c680312a0847a56efa48f230da440d435ef8a6b2f5a5c5e140476".hexStringToBytes.toArray,
        "7fcd9f9bd1c918654416cdcd2ed908a0f978c58f2123ae896d9df28b42c59367".hexStringToBytes.toArray
      )
    )
    val specOut_sig_1 = SignatureKesSum(
      ("9bef5555cb073c4b675fd182ca6d78dcfe19364dbeae5283a1888b42fbe55e3d".hexStringToBytes.toArray),
      (
        "099a205e1152bf98ded60373921a32dfe9a9f28fbfc09364bf533f7bef35d5e6c4fb000fb539bcae2a5913ef3c844d466b69c6f2e52c46bafe3275da53cc520a".hexStringToBytes.toArray
      ),
      Vector(
        "a5bef7f1979ed0d776fed04c50c27c221b2f834b369744b4b84bc10e742fd396".hexStringToBytes.toArray,
        "7fcd9f9bd1c918654416cdcd2ed908a0f978c58f2123ae896d9df28b42c59367".hexStringToBytes.toArray
      )
    )
    val specOut_sig_2 = SignatureKesSum(
      ("57d6a9536c0250183bc31d4e6fc2388a22fe498e57a6dc8a91fb8508694f6a3d".hexStringToBytes.toArray),
      (
        "2965bae2c6c968ed160523ba886af63f3ca82bd9f8d7ecbf31a8ea0fef8bd95b2afc2a2120c5ae31db2ef29fa322e7f2db31cdf6c0c392bb1be7c9743bba3b0b".hexStringToBytes.toArray
      ),
      Vector(
        "9183ed1e702fc2f4983bc461e551d80df3d21b4eb284448e9e35f8a5691d92da".hexStringToBytes.toArray,
        "615244e7bd05088fb462ba9e7b995d71129b91b42652c06780b17936eb456461".hexStringToBytes.toArray
      )
    )
    val specOut_sig_3 = SignatureKesSum(
      ("c3dcdaab1292b810b55db8758258824cd73bf913fc59a2f1038fa707bbaff3b6".hexStringToBytes.toArray),
      (
        "81de65fa1e27a33c006f173b2119aa190882d13034afc2ccd184c4a7e3549fc123bfa85be23438ac47fa22504b3253bf96bce3a1a849e9415e635581056d5f0c".hexStringToBytes.toArray
      ),
      Vector(
        "5b4b52bc5dd65cb4c8e6ab2faa906c595bc607fab3a1f9f17f2ccf5ddcf798a1".hexStringToBytes.toArray,
        "615244e7bd05088fb462ba9e7b995d71129b91b42652c06780b17936eb456461".hexStringToBytes.toArray
      )
    )

    val (sk, vk) = kesSum.createKeyPair(specIn_seed, specIn_height, 0)
    val sig_0 = kesSum.sign(sk, specIn_msg)
    val sk_1 = kesSum.update(sk, 1)
    val sig_1 = kesSum.sign(sk_1, specIn_msg)
    val vk_1 = kesSum.getVerificationKey(sk_1)
    val sk_2 = kesSum.update(sk_1, 2)
    val sig_2 = kesSum.sign(sk_2, specIn_msg)
    val vk_2 = kesSum.getVerificationKey(sk_2)
    val sk_3 = kesSum.update(sk_2, 3)
    val sig_3 = kesSum.sign(sk_3, specIn_msg)
    val vk_3 = kesSum.getVerificationKey(sk_3)
    vk shouldBe specOut_vk
    sig_0 shouldBe specOut_sig_0
    sig_1 shouldBe specOut_sig_1
    sig_2 shouldBe specOut_sig_2
    sig_3 shouldBe specOut_sig_3
    kesSum.verify(sig_0, specIn_msg, vk) shouldBe true
    kesSum.verify(sig_1, specIn_msg, vk_1) shouldBe true
    kesSum.verify(sig_2, specIn_msg, vk_2) shouldBe true
    kesSum.verify(sig_3, specIn_msg, vk_3) shouldBe true
  }

  it should "Test Vector - 3 : Generate and verify a specified sum composition signature at t = [0, 2, 5, 7] using a provided seed, message, and height" in {
    val kesSum = new KesSum()
    val specIn_seed = "36d37def07d10f7acb1570cca5b56237b3d5700fd4f5e5b5c44d6af09f2c2ffb".hexStringToBytes.toArray
    val specIn_height = 3
    val specIn_time = 0
    val specIn_msg = "6172726976696e67206f6e2074696d6520616e64206c617465".hexStringToBytes.toArray

    val specOut_vk =
      VerificationKeyKesSum(
        "deafadc6b9aa9a018871b54f5d9bfe6acaa3d1ad82e9fadefa347ae9fbc920e7".hexStringToBytes.toArray,
        specIn_time
      )
    val specOut_sig_0 = SignatureKesSum(
      ("79d37776daaf3a2952bdc9124d7228175f4f6fdd3fe83c42bddd4f61b68df1cc".hexStringToBytes.toArray),
      (
        "bcb07a0e68a5ad2b696b44a56a651a95e65885a6908c8fa92825f9ad510fd84cd4e0d67601cd06d17eae8554f5eac5cca817033b3d011924245589fc047db90d".hexStringToBytes.toArray
      ),
      Vector(
        "a5b68a7fad1976bc2f38a46279eca5624f303236f334663e08bbc876765c4533".hexStringToBytes.toArray,
        "581b77b90035b18faa4ea080cf7d653902d9c9903539f21156c948ec1a39d0fb".hexStringToBytes.toArray,
        "0ada5190f9f8a7ac5d8ba424dbc29ab4135444bbe459a77c598032d60c8d5b8e".hexStringToBytes.toArray
      )
    )
    val specOut_sig_1 = SignatureKesSum(
      ("8b0126047439415a5dba9e426bce4c1a991d37c5ffa1b43c1172fd6f83503069".hexStringToBytes.toArray),
      (
        "0e95cfa983c878cee7caa3f776cd2e67fd2c6145b2fd2d8fe1bc3a72ec6812e2319a821b805ddd680605638b8571efab78ae6af957359f01b1a5b1db19d12205".hexStringToBytes.toArray
      ),
      Vector(
        "e65e5c7a60a9e849bb1ce1f6ce2f6a5e018e8876f0842846555dc5ca78cdce8f".hexStringToBytes.toArray,
        "b22e36aad1b810dbcca01681cf9b4273633aa5467d3a36ce215f94f194c9a99b".hexStringToBytes.toArray,
        "0ada5190f9f8a7ac5d8ba424dbc29ab4135444bbe459a77c598032d60c8d5b8e".hexStringToBytes.toArray
      )
    )
    val specOut_sig_2 = SignatureKesSum(
      ("37a1dcaf5b889bdebf17f8c66a27b83a4a8dcf79fe8b0ab1e26c6852a29a6683".hexStringToBytes.toArray),
      (
        "9ed7112b8a92645c34f3271d181cc29b5a4a4e2bdf90a40abffa5f6020cb56993a880a97e094c2d673fa9a789b10979b17e649a2e9ac0b7bc7bc768404255001".hexStringToBytes.toArray
      ),
      Vector(
        "5e1923f93aff941e5d9a7b259c8589dbe088d4540aa729887781d27905002907".hexStringToBytes.toArray,
        "95e63597096d10330505e0d0c505c46dd0388c4a6cb6f033d060ece90c8ee966".hexStringToBytes.toArray,
        "2c8b7318be3bcf7963fed0545fcae11ab48d60163487a7cf3e8574c36e5d75e0".hexStringToBytes.toArray
      )
    )
    val specOut_sig_3 = SignatureKesSum(
      ("4fee4bf190c11d0cf7175550343da416928a12bc6d0feb2c889f513d58fd8217".hexStringToBytes.toArray),
      (
        "859a8e078f1f4b21303d33076c4ee0f1d1337bb5441837c66bd68f7518da98192b3c3361c1ece772adea47e09a0a230e814c64a75a6057b51cd0f7909d6c5a06".hexStringToBytes.toArray
      ),
      Vector(
        "a0b175ed12fecdc8b686da7d448101e8e407d8e33687288a638862bf62dd5e25".hexStringToBytes.toArray,
        "164717632cf3e0016d84dbaa60bd593e3bc50b7dfece3cc1adb79aa99583fc63".hexStringToBytes.toArray,
        "2c8b7318be3bcf7963fed0545fcae11ab48d60163487a7cf3e8574c36e5d75e0".hexStringToBytes.toArray
      )
    )

    val (sk, vk) = kesSum.createKeyPair(specIn_seed, specIn_height, 0)
    val sig_0 = kesSum.sign(sk, specIn_msg)
    val sk_1 = kesSum.update(sk, 2)
    val sig_1 = kesSum.sign(sk_1, specIn_msg)
    val vk_1 = kesSum.getVerificationKey(sk_1)
    val sk_2 = kesSum.update(sk_1, 5)
    val sig_2 = kesSum.sign(sk_2, specIn_msg)
    val vk_2 = kesSum.getVerificationKey(sk_2)
    val sk_3 = kesSum.update(sk_2, 7)
    val sig_3 = kesSum.sign(sk_3, specIn_msg)
    val vk_3 = kesSum.getVerificationKey(sk_3)
    vk shouldBe specOut_vk
    sig_0 shouldBe specOut_sig_0
    sig_1 shouldBe specOut_sig_1
    sig_2 shouldBe specOut_sig_2
    sig_3 shouldBe specOut_sig_3
    kesSum.verify(sig_0, specIn_msg, vk) shouldBe true
    kesSum.verify(sig_1, specIn_msg, vk_1) shouldBe true
    kesSum.verify(sig_2, specIn_msg, vk_2) shouldBe true
    kesSum.verify(sig_3, specIn_msg, vk_3) shouldBe true
  }

  it should "Test Vector - 4 : Generate and verify a specified sum composition signature at t = [0, 5, 10, 15] using a provided seed, message, and height" in {
    val kesSum = new KesSum()
    val specIn_seed = "351f09534baf61171893903ab6f122b82ff21dd775f3d853fcd52ee91cb40178".hexStringToBytes.toArray
    val specIn_height = 4
    val specIn_time = 0
    val specIn_msg = "776f6e6465722077686572652077696474682077656e74".hexStringToBytes.toArray

    val specOut_vk =
      VerificationKeyKesSum(
        "be127e4da6622de7785a221faa76ad008a0e1cb4f41ca2cd937554cc268ae7ce".hexStringToBytes.toArray,
        specIn_time
      )
    val specOut_sig_0 = SignatureKesSum(
      ("a9679e07331d0d007dcdc5097b742aa51a3a8b02ca0abd3fb627649fc0eadce7".hexStringToBytes.toArray),
      (
        "f0c469c5771b84b88f9dd08e43ef64e976cc0d38cb73b8c9517346106cbc122399f707a4e9aa505652e88da4059a2e52eec3bb6915ac46f6157d3d4c7e61ca00".hexStringToBytes.toArray
      ),
      Vector(
        "fd2a8bb21fff75a6db541253f5a48e652d882dba3475edabb5145c2d95a7f70e".hexStringToBytes.toArray,
        "551c64f9e4bcc5996f15ca8ab54d1f3d7fda472ac2174f9706332abe9940f7e5".hexStringToBytes.toArray,
        "640652610bfa34bdc65fed175f6a4997ac2b95aa7e948b6b5df2df5826d62ed9".hexStringToBytes.toArray,
        "b737aa8e49a4b642a1162e0124a39c3bba4d83fee4d3747816e38482e6d2ae86".hexStringToBytes.toArray
      )
    )
    val specOut_sig_1 = SignatureKesSum(
      ("f17086c76d2ba3c587f3bbbdf6cb457e2f25b6c7e5f0f7b53916a09991edb19c".hexStringToBytes.toArray),
      (
        "34d93cbd32f654862bdc5005def956f0791226f71b40f19cd1d656c9038ec7fa813426f3a261fec8f6645e5d4a3c8a6627ab8e41db88ba3fa383f6e01972400e".hexStringToBytes.toArray
      ),
      Vector(
        "1729eaa48403f75cc25f678f36467764fb3af1a926e30a6b27efbdaf0cf3d13e".hexStringToBytes.toArray,
        "8ff3f095b1d1f6d6727aa3197c7e3c2e5fb4db14d7b75e6694a40d5e1a757352".hexStringToBytes.toArray,
        "46e57576ca928be69a373b957e4f0641a886806f6c87ce0679096daedb311f32".hexStringToBytes.toArray,
        "b737aa8e49a4b642a1162e0124a39c3bba4d83fee4d3747816e38482e6d2ae86".hexStringToBytes.toArray
      )
    )
    val specOut_sig_2 = SignatureKesSum(
      ("57ba0d6fd664d30a5e4186b601c887a8e7fa9c78e50cd3351b704f1a3740a4c1".hexStringToBytes.toArray),
      (
        "d15c37f8c4fae4151fe3fa9ec5574a123dbbac849caa3c2ee124ad438aa8ff7a38eaf6da50940d42e1dff69dacb6977345125773822a8d97109a9029d8efab07".hexStringToBytes.toArray
      ),
      Vector(
        "74a6f25b5a9a99049f12c05abdbe5e852c767dbeb6ca79689b148434de8e3433".hexStringToBytes.toArray,
        "c95a8684e9590035520e33074985b2ab2f14b5f072fc3ad2c450d23bf4a08f58".hexStringToBytes.toArray,
        "215c74ddbd507b6227beaf1a13a53ee843b2fb2edaca856b67590cdb66c6d818".hexStringToBytes.toArray,
        "df602c5c6d0eafd2db7c6de7f0ee0ca72e12579450808c3827fb0cdc42836f63".hexStringToBytes.toArray
      )
    )
    val specOut_sig_3 = SignatureKesSum(
      ("7f31b22fd7c0240f48fd6ee684b66e7ef80a73c9fc92a38343c8add30c0c468c".hexStringToBytes.toArray),
      (
        "6796e393202e19a74a20f6d449f0704c1891a4957a42342303bdadc7543e6e298836ab9e1374281f4514ae7794e1273efd0c6f742987fd68296c4d7a07639007".hexStringToBytes.toArray
      ),
      Vector(
        "07e5324bafee617d5bfab36b357bf8bad39e1fdf41cd28bd0881e6dec45b10de".hexStringToBytes.toArray,
        "b0fb6589fe3b694650ec8c847f41b6525ef672632ede69cf0330c630ecd414ec".hexStringToBytes.toArray,
        "6d763b9fcf4de3c806d272b022fb48fb29e366de8da178fbf3fc55ddaf4a1e08".hexStringToBytes.toArray,
        "df602c5c6d0eafd2db7c6de7f0ee0ca72e12579450808c3827fb0cdc42836f63".hexStringToBytes.toArray
      )
    )

    val (sk, vk) = kesSum.createKeyPair(specIn_seed, specIn_height, 0)
    val sig_0 = kesSum.sign(sk, specIn_msg)
    val sk_1 = kesSum.update(sk, 5)
    val sig_1 = kesSum.sign(sk_1, specIn_msg)
    val vk_1 = kesSum.getVerificationKey(sk_1)
    val sk_2 = kesSum.update(sk_1, 10)
    val sig_2 = kesSum.sign(sk_2, specIn_msg)
    val vk_2 = kesSum.getVerificationKey(sk_2)
    val sk_3 = kesSum.update(sk_2, 15)
    val sig_3 = kesSum.sign(sk_3, specIn_msg)
    val vk_3 = kesSum.getVerificationKey(sk_3)
    vk shouldBe specOut_vk
    sig_0 shouldBe specOut_sig_0
    sig_1 shouldBe specOut_sig_1
    sig_2 shouldBe specOut_sig_2
    sig_3 shouldBe specOut_sig_3
    kesSum.verify(sig_0, specIn_msg, vk) shouldBe true
    kesSum.verify(sig_1, specIn_msg, vk_1) shouldBe true
    kesSum.verify(sig_2, specIn_msg, vk_2) shouldBe true
    kesSum.verify(sig_3, specIn_msg, vk_3) shouldBe true
  }

  it should "Test Vector - 5 : Generate and verify a specified sum composition signature at t = [0, 10, 21, 31] using a provided seed, message, and height" in {
    val kesSum = new KesSum()
    val specIn_seed = "c77bbf01a20dea8cfbd9acce52134a845c67bfd9b2cfa5c115f3a9c8597dcd03".hexStringToBytes.toArray
    val specIn_height = 5
    val specIn_time = 0
    val specIn_msg = "6d65206f72206974202d206569746865722077617920796f75206a756d70206f6666".hexStringToBytes.toArray

    val specOut_vk =
      VerificationKeyKesSum(
        "70c0d236d2892d1745b8c0547d93bb8ab48adbf7e4498af5dbc123a3fdf80e21".hexStringToBytes.toArray,
        specIn_time
      )
    val specOut_sig_0 = SignatureKesSum(
      ("bfb3bd77c3195566c00779a8a3be98e48687efb9fe0703fef255188abe3e8c86".hexStringToBytes.toArray),
      (
        "97ed7c65fce2ede533ab5476e43e2f627d3dbf2d6f0d8baeb971e7392c291fb10c4ec2b9103b7df5052fb4f7c7df0906d2c14b22770749781f85e95654e30e0b".hexStringToBytes.toArray
      ),
      Vector(
        "2398f25f7637ae13081ba4b7674b9123f54eab960ec751a0166d76dce379b84c".hexStringToBytes.toArray,
        "3ab1ccf0e083de049cfbe02e037d23ee8173af82531d8b4fd3a6ee2e4881be3d".hexStringToBytes.toArray,
        "47803271bc0b85a8bee2e04ad1a2dbe7b66457d04912c65909027f794d971038".hexStringToBytes.toArray,
        "a0d5cedc00c1993ea3bc06c5f8d196a1902266266d51390ef5bc15073a5be086".hexStringToBytes.toArray,
        "a5ae34468d39e8c5bbf8b9427444f1d507ce5678b5a73d4d2937b0dffe190f3e".hexStringToBytes.toArray
      )
    )
    val specOut_sig_1 = SignatureKesSum(
      ("2c3893ffb5eaa06b10baab53e2302103210465f4b02a947756040606c6047920".hexStringToBytes.toArray),
      (
        "dd689f98054fb4d85c747a808a196434051a67a52e51c405d49c6153f767ef8586669525a9ebde5a4d5395c220c48fae29e6088a9e8419b38b9189eb9a78d206".hexStringToBytes.toArray
      ),
      Vector(
        "14ec66d47c77c530382146ddffce027a7d14221f336f5ac3fc37cc369c1c19bf".hexStringToBytes.toArray,
        "c8c437996395666af653895d0b1e74b2bd8771bd0c96ec78d2f3a2c6a8ad434f".hexStringToBytes.toArray,
        "0c86200d9fbf50c21a5e03e2804ddacc19cb8b86e6ace577fa7813b39046c405".hexStringToBytes.toArray,
        "0e036b4627a99ce67f9e94452f6837018123b354654129e19f4dbb70429eb891".hexStringToBytes.toArray,
        "a5ae34468d39e8c5bbf8b9427444f1d507ce5678b5a73d4d2937b0dffe190f3e".hexStringToBytes.toArray
      )
    )
    val specOut_sig_2 = SignatureKesSum(
      ("d43476ed2380b1b96902bfaec96c1f675ad113bd55d86b506c9a26895a69b496".hexStringToBytes.toArray),
      (
        "cfd4e8a463ee8c32ca3781edeaefc4673b98631ad0b4ddfcf69baf2a5c6749734dea5401395e3c27063d1d44f13f911a7a2b738fd7bdb359ce7b4b99f8b8f90b".hexStringToBytes.toArray
      ),
      Vector(
        "c77f68fe69b444a67b5544d59635dd5391cf065849c5f443705be9f5f080d9f5".hexStringToBytes.toArray,
        "23273d7995ee43d27d4ab9320774707f50b6d2170f1648540167db548d1032ea".hexStringToBytes.toArray,
        "b8439adb5dec5d6c8775cfadfc6568bd3682a08a5d328452b7823ceeff59a984".hexStringToBytes.toArray,
        "8fab6b378cfa4554103345586bdf2db37897c9fdc91e0d378e16bfced6f44d82".hexStringToBytes.toArray,
        "7feb24f2a47efff49217759e3e76af0def79946407710c243c19e0b4f131b326".hexStringToBytes.toArray
      )
    )
    val specOut_sig_3 = SignatureKesSum(
      ("decaf6e8f7f6c767b096775950d8f551b47f3975d5ec196d41f521850722d2c2".hexStringToBytes.toArray),
      (
        "c1e8335f8a5667138839f331310b930fddcdd61cf801a4337949e6e9470bec6ca087d26737f5641aa7ff7951abd1ed5fd5bee610763c0ea24aff7ef9dfc37809".hexStringToBytes.toArray
      ),
      Vector(
        "4179e8cf03735b99189a37614801e8f5f608c6bd11878fd24fffc47da7b54dd0".hexStringToBytes.toArray,
        "781125c39719d6c58c554d1b0203aa681db2bedf056c65897fec0faa18c770d9".hexStringToBytes.toArray,
        "9cd7efa15e265d6398ca38b482ac117584c1a8bac10a39415b6fb9462b69d978".hexStringToBytes.toArray,
        "6056f6e9d54b77c4c4d29b7588881938859bab939a7f7dea21d59804683edbc7".hexStringToBytes.toArray,
        "7feb24f2a47efff49217759e3e76af0def79946407710c243c19e0b4f131b326".hexStringToBytes.toArray
      )
    )

    val (sk, vk) = kesSum.createKeyPair(specIn_seed, specIn_height, 0)
    val sig_0 = kesSum.sign(sk, specIn_msg)
    val sk_1 = kesSum.update(sk, 10)
    val sig_1 = kesSum.sign(sk_1, specIn_msg)
    val vk_1 = kesSum.getVerificationKey(sk_1)
    val sk_2 = kesSum.update(sk_1, 21)
    val sig_2 = kesSum.sign(sk_2, specIn_msg)
    val vk_2 = kesSum.getVerificationKey(sk_2)
    val sk_3 = kesSum.update(sk_2, 31)
    val sig_3 = kesSum.sign(sk_3, specIn_msg)
    val vk_3 = kesSum.getVerificationKey(sk_3)
    vk shouldBe specOut_vk
    sig_0 shouldBe specOut_sig_0
    sig_1 shouldBe specOut_sig_1
    sig_2 shouldBe specOut_sig_2
    sig_3 shouldBe specOut_sig_3
    kesSum.verify(sig_0, specIn_msg, vk) shouldBe true
    kesSum.verify(sig_1, specIn_msg, vk_1) shouldBe true
    kesSum.verify(sig_2, specIn_msg, vk_2) shouldBe true
    kesSum.verify(sig_3, specIn_msg, vk_3) shouldBe true
  }
}
