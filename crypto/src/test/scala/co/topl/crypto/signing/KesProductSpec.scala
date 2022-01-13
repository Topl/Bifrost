package co.topl.crypto.signing

import co.topl.crypto.utils.Generators.{genBytesWithBoundedSize, genRandomlySizedBytes}
import co.topl.crypto.utils.Hex.implicits._
import co.topl.crypto.utils.KesTestHelper
import co.topl.models.{Bytes, Proofs, SecretKeys, VerificationKeys}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class KesProductSpec
    extends AnyFlatSpec
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  "KesProduct" should "verify a message signed with the appropriate public key" in {
    forAll(
      genRandomlySizedBytes,
      genRandomlySizedBytes,
      genRandomlySizedBytes,
      genRandomlySizedBytes,
      Gen.choose(1, 12),
      Gen.choose(1, 12)
    ) { (seed1: Bytes, seed2: Bytes, message1: Bytes, message2: Bytes, supHeight: Int, subHeight: Int) =>
      whenever(!(seed1 == seed2) && !(message1 == message2)) {
        val kesProduct = new KesProduct
        val (sk1, vk1) = kesProduct.createKeyPair(seed1, (supHeight, subHeight), 0)
        val (_, vk2) = kesProduct.createKeyPair(seed2, (supHeight, subHeight), 0)
        val sig = kesProduct.sign(sk1, message1)

        kesProduct.verify(sig, message1, vk1) shouldBe true
        kesProduct.verify(sig, message1, vk2) shouldBe false
        kesProduct.verify(sig, message2, vk1) shouldBe false
      }
    }
  }

  it should "generate identical keypairs given the same seed" in {
    forAll(genBytesWithBoundedSize(1, 1024), Gen.choose(1, 12), Gen.choose(1, 12)) {
      (seedBytes: Bytes, supHeight: Int, subHeight: Int) =>
        val kesProduct = new KesProduct
        val (_, vk1) = kesProduct.createKeyPair(seedBytes, (supHeight, subHeight), 0)
        val (_, vk2) = kesProduct.createKeyPair(seedBytes, (supHeight, subHeight), 0)

        vk1 shouldBe vk2
    }
  }

  it should "test private key 1 - generate the correct private key at a given time step" in {
    val kesProduct = new KesProduct()
    val specIn_seed = "38c2775bc7e6866e69c6acd5e12ee366fd57f7df1b30e200cae610ec4ecf378c".hexStringToBytes
    val specIn_height = (1, 2)
    val specIn_time = 0
    val specOut_vk = VerificationKeys.KesProduct(
      "56d4f5dc6bfe518c9b6898222c1bfc97e93f760ec48df07704369bc306884bdd".unsafeStrictBytes,
      specIn_time
    )

    val specOut_sk: SecretKeys.KesProduct = SecretKeys.KesProduct(
      KesTestHelper.PrivateKeyConstructor.build(
        "0000000000000000000000000000000000000000000000000000000000000000".hexStringToBytes,
        "9077780e7a816f81b2be94b9cbed9248db8ce03545819387496047c6ad251f09".hexStringToBytes,
        (
          true,
          (
            "0000000000000000000000000000000000000000000000000000000000000000".hexStringToBytes,
            "9ec328f26f8a298c8dfd365d513301b316c09f423f111c4ab3cc84277bb1bafc".hexStringToBytes,
            "377b3bd79d099313a59dbac4fcb74cd9b45bfe6e32030e90c8f4a1dfae3bc986".hexStringToBytes
          )
        )
      ),
      KesTestHelper.PrivateKeyConstructor.build(
        "57185fdef1032136515d53e1b104acbace7d9b590465c9b11a72c8943f02c7a4".hexStringToBytes,
        "d7cab746d246b5fc21b40b8778e377456a62d03636e10a0228856d61453c7595".hexStringToBytes,
        (
          true,
          (
            "0000000000000000000000000000000000000000000000000000000000000000".hexStringToBytes,
            "330daba116c2337d0b5414cc46a73506d709416c61554722b78b0b66e765443b".hexStringToBytes,
            "652c7e4997aa62a06addd75ad8a5c9d54dc9479bbb1f1045c5e5246c83318b92".hexStringToBytes
          )
        ),
        (
          false,
          (
            "c32eb1c5e9bcd3d96243e6371f52781a4f6ac6dac6976f26544c99d31f5dbecb".hexStringToBytes,
            "3726a93ad80a90eb8ef9abb49cfd954b7658fd5eb14d65e1b9b57d77253321dc".hexStringToBytes,
            "9d1f9f9d03b6ed90710c7eaf2d9156a3b34a290d7baf79e775b417336a4415d1".hexStringToBytes
          )
        )
      ),
      "d82ab9526323833262ac56f65860f38faa433ff6129c24f033e6ea786fd6db6b".unsafeStrictBytes,
      Proofs.Signature.KesSum(
        VerificationKeys.Ed25519("9077780e7a816f81b2be94b9cbed9248db8ce03545819387496047c6ad251f09".unsafeStrictBytes),
        Proofs.Signature.Ed25519(
          "cb7af65595938758f60009dbc7312c87baef3f8f88a6babc01e392538ec331ef20766992bc91b52bedd4a2f021bbd9e10f6cd8548dd9048e56b9579cf975fe06".unsafeStrictBytes
        ),
        Vector(
          "9ec328f26f8a298c8dfd365d513301b316c09f423f111c4ab3cc84277bb1bafc".unsafeStrictBytes
        )
      ),
      0L
    )
    val (sk, vk) = kesProduct.createKeyPair(specIn_seed, specIn_height, 0)
    val sk_t = kesProduct.update(sk, 6)
    vk shouldBe specOut_vk
    KesTestHelper.areEqual(sk_t, specOut_sk) shouldBe true
  }

  it should "test private key 2 - generate the correct private key at a given time step" in {
    val kesProduct = new KesProduct()
    val specIn_seed = "768f9760f74bab4f56fa46b7030525f227368d553c1b6de57026b675bdd6295a".hexStringToBytes
    val specIn_height = (4, 4)
    val specIn_time = 0
    val specOut_vk = VerificationKeys.KesProduct(
      "3cef287de1467276a6527f3af9f2e199aaf0efa1d0f801cc69cd7e340580fd89".unsafeStrictBytes,
      specIn_time
    )

    val specOut_sk = SecretKeys.KesProduct(
      KesTestHelper.PrivateKeyConstructor.build(
        "0000000000000000000000000000000000000000000000000000000000000000".hexStringToBytes,
        "19f5224ed6c6ac6fd7f6a532491befb22b29164a3af74c33bd78dcefe0f5e68b".hexStringToBytes,
        (
          false,
          (
            "e781753461ecb4afecb94997a8473a45f6261fd78fef170d6557a3a69140ca55".hexStringToBytes,
            "d51207e5a59e22e6a55584766942a433426dd1582a2aeaaa749b63cfba8fc436".hexStringToBytes,
            "3eb00d5169d07be9cfe6684a7798ecc3af0fb196dd0d108dfa2bbf051a37e061".hexStringToBytes
          )
        ),
        (
          true,
          (
            "0000000000000000000000000000000000000000000000000000000000000000".hexStringToBytes,
            "f1a7b96bf5fc14b1414d0d2d421352533bf7d84a07fb58d5e0859624343b4709".hexStringToBytes,
            "e0227b25fdc6cc314e2bd448a16aaef620e298636cd2ea1689e07ef52a9410d8".hexStringToBytes
          )
        ),
        (
          false,
          (
            "54d10a2bd0cf11dded3f509941212870e6d4f940902e9cca748bc54b2055c7b1".hexStringToBytes,
            "e5cc48d270133fde59bc64d39504bd5ad57c5261f5ea34e871990e9a2e2f7475".hexStringToBytes,
            "d67d7e5745c1bea953b1d21131d792295db7f6eef4684e5ecc3c6f75f1a2b5a4".hexStringToBytes
          )
        ),
        (
          true,
          (
            "0000000000000000000000000000000000000000000000000000000000000000".hexStringToBytes,
            "5c4d66136f7b09228f6cfbf1bc8c30cd4b247d4456df869d591ddff1130f40eb".hexStringToBytes,
            "6256516657d4f97667e1ebdf899756a39d6578f48249fdece6ead08e93df0043".hexStringToBytes
          )
        )
      ),
      KesTestHelper.PrivateKeyConstructor.build(
        "cee76653078945c1387f325784f63871b7e1a384927548d2fb9a76ba50ffc01b".hexStringToBytes,
        "cd77c681303d3c3e83094c0dc825bce88395ec4a6eab4a9ce84d7cb0944c3061".hexStringToBytes,
        (
          false,
          (
            "f85d7cd4630684d6f1cf1de625df020b34ae97f0c8239559efca2310d634178d".hexStringToBytes,
            "2f67dbbcc219e32233804216fc330bb694c5215db4dc62adc89adf99057e55a5".hexStringToBytes,
            "4d4e98f9c98c53cb6b37af3a3b5dd51d127523fd1673efe43e3e92b68da4dee0".hexStringToBytes
          )
        ),
        (
          true,
          (
            "0000000000000000000000000000000000000000000000000000000000000000".hexStringToBytes,
            "6d0f9e5306bc153a4a63485af566cd420625f38f4c2f909e9256390ca18769f2".hexStringToBytes,
            "9e788eeb57c2542e8efee7c9487c44b6fcba8a591176a6857f9196135bb6ddee".hexStringToBytes
          )
        ),
        (
          false,
          (
            "7ace7c3d8e3e7ef99e4833fac6074cbc23792be189304a5190540dc107a03b91".hexStringToBytes,
            "1e3014b273fcbae4f48cb9e904e5af44b543b9ff23d173c7964dce5a31df8e86".hexStringToBytes,
            "4b651acd0fab5573a2fed3be1d11cb46d680d63ca687756c557c0bc5824b2723".hexStringToBytes
          )
        ),
        (
          true,
          (
            "0000000000000000000000000000000000000000000000000000000000000000".hexStringToBytes,
            "45e713f37f42f8dbe0b853e212d8fd3104a4ffc128aaabae49e2abfaeae68d09".hexStringToBytes,
            "ebeb2d1b5279763f43df3def888f24c86455ee21ba914cec5fb51a4d12e5e7a1".hexStringToBytes
          )
        )
      ),
      "8bba45a219cd212097ef60d275f2df3148ef848a91bdef24bb90d496be4e3a90".unsafeStrictBytes,
      Proofs.Signature.KesSum(
        VerificationKeys.Ed25519("19f5224ed6c6ac6fd7f6a532491befb22b29164a3af74c33bd78dcefe0f5e68b".unsafeStrictBytes),
        Proofs.Signature.Ed25519(
          "55788caf92e501bc5dc3bacc08aa898c0d3b88c84c835193e29d4dd33e1dddfde5b042bcbc8d76b8fabc524c034cdc1a158afaded22e6abccf886c5d8148d806".unsafeStrictBytes
        ),
        Vector(
          "5c4d66136f7b09228f6cfbf1bc8c30cd4b247d4456df869d591ddff1130f40eb".unsafeStrictBytes,
          "d67d7e5745c1bea953b1d21131d792295db7f6eef4684e5ecc3c6f75f1a2b5a4".unsafeStrictBytes,
          "f1a7b96bf5fc14b1414d0d2d421352533bf7d84a07fb58d5e0859624343b4709".unsafeStrictBytes,
          "3eb00d5169d07be9cfe6684a7798ecc3af0fb196dd0d108dfa2bbf051a37e061".unsafeStrictBytes
        )
      ),
      0L
    )
    val (sk, vk) = kesProduct.createKeyPair(specIn_seed, specIn_height, 0)
    val sk_t = kesProduct.update(sk, 85)
    vk shouldBe specOut_vk
    KesTestHelper.areEqual(sk_t, specOut_sk) shouldBe true
  }

  it should "Test Vector - 1 - Generate and verify a specified product composition signature at t = [0, 1, 2, 3] using a provided seed, message, and heights of the two trees" in {
    val kesProduct = new KesProduct()
    val specIn_seed = "2a6367c85f416ccef46a4521004228f74f24f7b0770ecced07c0dc035135bf6f".hexStringToBytes
    val specIn_height = (1, 1)
    val specIn_time = 0
    val specIn_msg = "697420617320646f206265206974206865206d65206f72".hexStringToBytes

    val specOut_vk = VerificationKeys.KesProduct(
      "47099c36fc71c2aae79046c65bb5d3f2c79d058bddd346370bfd22c6263d438d".unsafeStrictBytes,
      specIn_time
    )

    val specOut_sig_0 = Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("d1a38e6db07062c9c58036c537d1c999b6fc8b60c51feeede25afda66ee36395".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "e23e79815bf3faa96c786a2e7a22379b0f14d578aec4b2d31c225bd145dfc0b7041fd4a4dfff26f5214a168eccd9f416fdaeba6cf15784cc7451562550904109".unsafeStrictBytes
        ),
        Vector(
          "babb43bba46bb63fc0d7f0c460733f1835b23ec59cbf89b42b8ee5090616ba2e".unsafeStrictBytes
        )
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("1747b2fffbeccefa8a855bb28af7f7e8937bc5e24972bf49a9ad1cf26168ef54".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "4b6fb021e285e7f7408c3c80bd8217e8edcb5623e02f12956d32d9412caa1e995dd1400e3a638280aeba1aed909be2021c48d63dd966be1b52012b5ad392740b".unsafeStrictBytes
        ),
        Vector(
          "07586219db5738b54be24aec04f8f51f1d84a1531860135f9bcc4fbe3bc51a55".unsafeStrictBytes
        )
      ),
      "d86d2201174bea618cdae1f62f0be718e9cff353dea2d4da710652d2011727a4".unsafeStrictBytes
    )

    val specOut_sig_1 = Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("d1a38e6db07062c9c58036c537d1c999b6fc8b60c51feeede25afda66ee36395".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "e23e79815bf3faa96c786a2e7a22379b0f14d578aec4b2d31c225bd145dfc0b7041fd4a4dfff26f5214a168eccd9f416fdaeba6cf15784cc7451562550904109".unsafeStrictBytes
        ),
        Vector(
          "babb43bba46bb63fc0d7f0c460733f1835b23ec59cbf89b42b8ee5090616ba2e".unsafeStrictBytes
        )
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("d21551b12cf35d9b6022742352d6d5574b4f07f2cbab7f4cff25e43028b46aba".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "0517a4e29b8196ddfc1761fa2224b70cfc1b1b80b96faed4826a0aee80dfb1da770fe310fdf5cd596f5ad34920e6eff15fe02d5fa4fbae79e2d3db1fee68cb02".unsafeStrictBytes
        ),
        Vector(
          "a2fc1f14fa2c724cf2972d030fca6f1adf78490018760fa3e3b5ad46279b47e1".unsafeStrictBytes
        )
      ),
      "d86d2201174bea618cdae1f62f0be718e9cff353dea2d4da710652d2011727a4".unsafeStrictBytes
    )

    val specOut_sig_2 = Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("4e66fe180d5cd03c1593d2295cb21e4fbbca8d0c5fe7dbf3372a9ee6f9f1f8ae".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "e69d7a6ae2164e20487400d4fb10d96a2fdb37501462bb5baf7a3cc0682f7eae95aab47c5eb6a9772a77768627a36641a47c92baca75cdde404e9bfae0301d02".unsafeStrictBytes
        ),
        Vector(
          "9417bf4a4456a865a92ec1cc0c50bf0f90d1f1c09f79ab80485a48ca975375e1".unsafeStrictBytes
        )
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("ec4a9f15574d260f9c41b0c847c08237a3f590e21bd9267bc72f25d1476ba338".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "7851eac4378709e78f8fc0cf17ddc7b6883670a31d1fb5fdd45b1f3a656988cd60d13c7b93095f9ce4b0e5c48dd79282761e542bfa187ff7c09619af5c7ef305".unsafeStrictBytes
        ),
        Vector(
          "d21e09026215b55fc6c295a7fb239f3c37b558a5a19cdcb10fefc9b65201d8df".unsafeStrictBytes
        )
      ),
      "4f9618b4e9bdfecb43d5038cee3f1eea092a29244ee7d36da3d1a828efe8efdb".unsafeStrictBytes
    )

    val specOut_sig_3 = Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("4e66fe180d5cd03c1593d2295cb21e4fbbca8d0c5fe7dbf3372a9ee6f9f1f8ae".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "e69d7a6ae2164e20487400d4fb10d96a2fdb37501462bb5baf7a3cc0682f7eae95aab47c5eb6a9772a77768627a36641a47c92baca75cdde404e9bfae0301d02".unsafeStrictBytes
        ),
        Vector(
          "9417bf4a4456a865a92ec1cc0c50bf0f90d1f1c09f79ab80485a48ca975375e1".unsafeStrictBytes
        )
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("be533ba32cdc053b51218abc6ce3ebe66a9c0aa6f8be97930d9abe0b370d264e".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "1fd2812b224d955f2444d1d8988704c83cee284c1f023b4f9696ef2d0c19db69c75379e3a470658c67cb308dcc72f3c2e89825be9363cae3e0c91020f495e90f".unsafeStrictBytes
        ),
        Vector(
          "af810b4355fc79ca2e1b27285a6dde3b725404b3937c7e120c499415375e79d8".unsafeStrictBytes
        )
      ),
      "4f9618b4e9bdfecb43d5038cee3f1eea092a29244ee7d36da3d1a828efe8efdb".unsafeStrictBytes
    )

    val (sk, vk) = kesProduct.createKeyPair(specIn_seed, specIn_height, 0)
    val sig_0 = kesProduct.sign(sk, specIn_msg)
    val sk_1 = kesProduct.update(sk, 1)
    val vk_1 = kesProduct.getVerificationKey(sk_1)
    val sig_1 = kesProduct.sign(sk_1, specIn_msg)
    val sk_2 = kesProduct.update(sk_1, 2)
    val vk_2 = kesProduct.getVerificationKey(sk_2)
    val sig_2 = kesProduct.sign(sk_2, specIn_msg)
    val sk_3 = kesProduct.update(sk_2, 3)
    val vk_3 = kesProduct.getVerificationKey(sk_3)
    val sig_3 = kesProduct.sign(sk_3, specIn_msg)

    vk shouldBe specOut_vk
    sig_0 shouldBe specOut_sig_0
    sig_1 shouldBe specOut_sig_1
    sig_2 shouldBe specOut_sig_2
    sig_3 shouldBe specOut_sig_3
    kesProduct.verify(sig_0, specIn_msg, vk) shouldBe true
    kesProduct.verify(sig_1, specIn_msg, vk_1) shouldBe true
    kesProduct.verify(sig_2, specIn_msg, vk_2) shouldBe true
    kesProduct.verify(sig_3, specIn_msg, vk_3) shouldBe true
  }

  it should "Test Vector - 2 - Generate and verify a specified product composition signature at t = [0, 2, 4, 6] using a provided seed, message, and heights of the two trees" in {
    val kesProduct = new KesProduct()
    val specIn_seed = "38c2775bc7e6866e69c6acd5e12ee366fd57f7df1b30e200cae610ec4ecf378c".hexStringToBytes
    val specIn_height = (1, 2)
    val specIn_time = 0
    val specIn_msg =
      "696e20612073686f7274207768696c65206372616674202d206e6f74206d75636820746f20646f2061626f7574206974".hexStringToBytes

    val specOut_vk = VerificationKeys.KesProduct(
      "56d4f5dc6bfe518c9b6898222c1bfc97e93f760ec48df07704369bc306884bdd".unsafeStrictBytes,
      specIn_time
    )
    val specOut_sig_0 = Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("5b9fe44c021bdd9c3b34f600efdd73c795fccd0604a124ca8a3d4d24a2a20b88".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "0860e791a13f1a996339054810b531ae7e7fc8a67336829dbe7698f3cd7e2f1334a020f0ff366a60e83f1968df726cd3d07c0343da1f003d588add43662fab0b".unsafeStrictBytes
        ),
        Vector(
          "377b3bd79d099313a59dbac4fcb74cd9b45bfe6e32030e90c8f4a1dfae3bc986".unsafeStrictBytes
        )
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("9705690aef09bbbf05255024892366952a9f71a9c671ad3f4138043dce5f6793".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "fe5433ccec7b16d829acafdf3a495771fea4e2a82885a8a89b71a2cfee1db724914e5f8acd4c52e9547059154c5a5ebadde374eae2fdcb546df49941a0780e0d".unsafeStrictBytes
        ),
        Vector(
          "7fbea3be88d776943503ea05a9698e9b43d221f9d4a7f65ee62ecea21381859c".unsafeStrictBytes,
          "37ce2848ce72147cc9a42268034c2a781168b932fae3a92964795dd993fe30ca".unsafeStrictBytes
        )
      ),
      "42c2e48e9fb057643092f93e9285ef51b993cc4b9724ffef429ade875010555b".unsafeStrictBytes
    )

    val specOut_sig_1 = Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("5b9fe44c021bdd9c3b34f600efdd73c795fccd0604a124ca8a3d4d24a2a20b88".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "0860e791a13f1a996339054810b531ae7e7fc8a67336829dbe7698f3cd7e2f1334a020f0ff366a60e83f1968df726cd3d07c0343da1f003d588add43662fab0b".unsafeStrictBytes
        ),
        Vector(
          "377b3bd79d099313a59dbac4fcb74cd9b45bfe6e32030e90c8f4a1dfae3bc986".unsafeStrictBytes
        )
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("3841939979fc0bd3f99f4aa16d6784c2645e61717e26e8ef546c4c5161c94eea".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "20c90ce415ad35966203e763aeb52abc163117ebb6e22ceb9c2fb7cecdeec899c34fa00c9ccc56667586667dcb96f24a014456f98857cf5508a96dae2dfb4b04".unsafeStrictBytes
        ),
        Vector(
          "855282b123c25f86405d5508dbad51480c80ab4eded37c3ae59705bcbbd36c6a".unsafeStrictBytes,
          "9b0df1a883f526ef04b80ea74031ac17781ec0954f7796e782c315d4617b9f8b".unsafeStrictBytes
        )
      ),
      "42c2e48e9fb057643092f93e9285ef51b993cc4b9724ffef429ade875010555b".unsafeStrictBytes
    )

    val specOut_sig_2 = Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("9077780e7a816f81b2be94b9cbed9248db8ce03545819387496047c6ad251f09".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "cb7af65595938758f60009dbc7312c87baef3f8f88a6babc01e392538ec331ef20766992bc91b52bedd4a2f021bbd9e10f6cd8548dd9048e56b9579cf975fe06".unsafeStrictBytes
        ),
        Vector(
          "9ec328f26f8a298c8dfd365d513301b316c09f423f111c4ab3cc84277bb1bafc".unsafeStrictBytes
        )
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("bb640ed46e13cf24f5dcbb1af1fdccb71e506e239c3c2c4258cb38a13d00d248".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "0377b3e58df3f2b64718a0ad0d1fe146d79b0be361131c7aa965b6448dffcc8391f804f2956a762f63e9f2f4fca308d09baf4f7b70f6b27a5d358f78c7603109".unsafeStrictBytes
        ),
        Vector(
          "4928f7904341021fc080c2bdb8b45094661ddd6ad3655dc9b44ec5f1096d606d".unsafeStrictBytes,
          "652c7e4997aa62a06addd75ad8a5c9d54dc9479bbb1f1045c5e5246c83318b92".unsafeStrictBytes
        )
      ),
      "e6cc585ccec0d6d27aadacaea854fe921c6f0ccf5009a506570e7f2b8f704bac".unsafeStrictBytes
    )

    val specOut_sig_3 = Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("9077780e7a816f81b2be94b9cbed9248db8ce03545819387496047c6ad251f09".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "cb7af65595938758f60009dbc7312c87baef3f8f88a6babc01e392538ec331ef20766992bc91b52bedd4a2f021bbd9e10f6cd8548dd9048e56b9579cf975fe06".unsafeStrictBytes
        ),
        Vector(
          "9ec328f26f8a298c8dfd365d513301b316c09f423f111c4ab3cc84277bb1bafc".unsafeStrictBytes
        )
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("d7cab746d246b5fc21b40b8778e377456a62d03636e10a0228856d61453c7595".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "78eeeff16ea30306c64bf176cbac97005a35d88855f9f2ff283b990a45545297ba355644471d63fc6b5c21b5f26262db5d6317a45643383b9adfbe3f9ce0d504".unsafeStrictBytes
        ),
        Vector(
          "9d1f9f9d03b6ed90710c7eaf2d9156a3b34a290d7baf79e775b417336a4415d1".unsafeStrictBytes,
          "330daba116c2337d0b5414cc46a73506d709416c61554722b78b0b66e765443b".unsafeStrictBytes
        )
      ),
      "e6cc585ccec0d6d27aadacaea854fe921c6f0ccf5009a506570e7f2b8f704bac".unsafeStrictBytes
    )

    val (sk, vk) = kesProduct.createKeyPair(specIn_seed, specIn_height, 0)
    val sig_0 = kesProduct.sign(sk, specIn_msg)
    val sk_1 = kesProduct.update(sk, 2)
    val vk_1 = kesProduct.getVerificationKey(sk_1)
    val sig_1 = kesProduct.sign(sk_1, specIn_msg)
    val sk_2 = kesProduct.update(sk_1, 4)
    val vk_2 = kesProduct.getVerificationKey(sk_2)
    val sig_2 = kesProduct.sign(sk_2, specIn_msg)
    val sk_3 = kesProduct.update(sk_2, 6)
    val vk_3 = kesProduct.getVerificationKey(sk_3)
    val sig_3 = kesProduct.sign(sk_3, specIn_msg)

    vk shouldBe specOut_vk
    sig_0 shouldBe specOut_sig_0
    sig_1 shouldBe specOut_sig_1
    sig_2 shouldBe specOut_sig_2
    sig_3 shouldBe specOut_sig_3
    kesProduct.verify(sig_0, specIn_msg, vk) shouldBe true
    kesProduct.verify(sig_1, specIn_msg, vk_1) shouldBe true
    kesProduct.verify(sig_2, specIn_msg, vk_2) shouldBe true
    kesProduct.verify(sig_3, specIn_msg, vk_3) shouldBe true
  }

  it should "Test Vector - 3 - Generate and verify a specified product composition signature at t = [1, 3, 5, 7] using a provided seed, message, and heights of the two trees" in {
    val kesProduct = new KesProduct()
    val specIn_seed = "450daa6ca8aefdba78c142a659c438d1347e76e11e665237c9aae429f175789f".hexStringToBytes
    val specIn_height = (2, 1)
    val specIn_time = 0
    val specIn_msg =
      "7475726e20612073696e676c65206c6574746572202d206974206265636f6d657320612062697264206f72206c61746572".hexStringToBytes

    val specOut_vk = VerificationKeys.KesProduct(
      "74d3dc1319f1369d0149ef5229a07f7b5d057a3e30424e078da26b6df834640d".unsafeStrictBytes,
      specIn_time
    )

    val specOut_sig_0 = Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("f51de133e7d8f7b4790bc8c2db4fd557dc0da824447b60e47c57d8e06f7e1d41".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "3cd33a45dd2386fe0ac4e0dd7ed22eb1c7c5019a9fb274fc955903be9d6641fd65c615eec1f5e9b4087a3f79c8a55b95b3b37c42eeda4768a544f6e29b6a7105".unsafeStrictBytes
        ),
        Vector(
          "b21f6d6dc22977a73fa86a66d1ddf05b7e6b0ea9da2ac491778d053daef39bc6".unsafeStrictBytes,
          "91b5dbb3ae9c2d4aad0434490af4003a371ab53e5d6635f9575c0bbd77658de6".unsafeStrictBytes
        )
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("48b7b1379d58e0b6cf33c1d240d1a1ae38cf56cf7f259509e11bed0d199a15a2".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "94a26397daf003127a1d795bfba0a0829c448479df00d8f99f76fc2fc9fe4fec350f7741e04353048d7dd47d28abaaa904ab10b1616c924aa14dcf727a350a0b".unsafeStrictBytes
        ),
        Vector(
          "da115d8fd2f79f5819509db22417667f1aa4490b71e1bb2a99f6a185d380df0e".unsafeStrictBytes
        )
      ),
      "deedcfcb2fac9e445bb03977e2423361d2b6d87d8cb2f39ace46dadfc3299cd9".unsafeStrictBytes
    )

    val specOut_sig_1 = Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("3cc93037196a6f9cb79fec9604a69af4fde2ed9ec905148073bca84169d9146b".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "600295ba73124ed8a4443359adf1b45f85232d5f7d6c7ae67b5f09c44d2349dbe949dc302aee12920fbceba428b8e39a9975ee6301a6131eb011da4c2304700d".unsafeStrictBytes
        ),
        Vector(
          "4a127e3e2b43565538c05ad7076748e9c0cee288018334d705f064894ab9a41e".unsafeStrictBytes,
          "91b5dbb3ae9c2d4aad0434490af4003a371ab53e5d6635f9575c0bbd77658de6".unsafeStrictBytes
        )
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("46fb38ce7bc9a3754418560ee5e850fb9af2da99795d16f250b00428b5760bb0".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "80dc82e18cdc0e54adfff50dda29611fb0cecd2597bdb2f936ca6f30e852c93d99388a99565667017c4fbd72d414df356e2fb363411d72f6a7448f4e07f43b04".unsafeStrictBytes
        ),
        Vector(
          "8f2b04db04d1fcfc38391250fdd13b8621b62423084a489c1401a6f9e3ffe0f5".unsafeStrictBytes
        )
      ),
      "a112acbf7a72644c2e74b364409744ee34dc2c7f84eb0a99a5af9a6baa851163".unsafeStrictBytes
    )

    val specOut_sig_2 = Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("459d6fba233ad916b9d74bc07b4820d5055423079e28eeb798c305e6295e82c3".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "fb5180c093045ea779393752018487a59e4bc9a0b6dee0f74c980ef2aace789f14d5bd82f03c160c9a9195bf1854ff7b5ac4abed8cf7b602853aafac801d9f06".unsafeStrictBytes
        ),
        Vector(
          "bc820f64dc8dce378cd10bea0c8a1f806c86b99d8684842fb331e90b5c4d8fb0".unsafeStrictBytes,
          "137bb3f95c0139dd95a7126615c9893651cc6d47c479de3c14380671f26dc939".unsafeStrictBytes
        )
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("198e0e28ab19b8aeab5eb4b11bc5e27457842164b4074945387fb025c8343ec4".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "26602caca9b69271c4cfd25b3c5aa45fb8430ac6ebc0bc8f59620d2637cfa3e368fc27943d2a4b7c3f9284c425673b78c506667bd2646db6da7af1066a344202".unsafeStrictBytes
        ),
        Vector(
          "b6d54f7126ac52aa5737a713a5ef796b381a802a8a7fdca80ae20024b73dc676".unsafeStrictBytes
        )
      ),
      "86799e1b092036bed0ddb206b192ece76ce2f43447e52bb1932dcefcd41abb55".unsafeStrictBytes
    )

    val specOut_sig_3 = Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("7b082ecba5a1a18654d8c74e2c0748f521481376f500f47906f80e7c845e1503".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "fe9ebb1a0392e67c7b26561f9bb5c37e9aaf3a9083ab8634fb1359fb889b394a2b13d635036748fe37fee88b75804bc04d2e60bfdaf95e8a86f6f4af17166a06".unsafeStrictBytes
        ),
        Vector(
          "7158920099352e213b5c9d696e74393f72f1a71126aa831d04c6da61c51647c3".unsafeStrictBytes,
          "137bb3f95c0139dd95a7126615c9893651cc6d47c479de3c14380671f26dc939".unsafeStrictBytes
        )
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("bf6aff9ba35ff856d7852b66bed6fa7f08641a073f1a01d684443ad93b01abb1".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "e7f42394a7805b37def9e22deafd3d8e46f7b1e3411a384db68a7f9b47e97deca0a897a276cd4c52efd3133576e5ca2616d1b95e42a6eb016a173c9f3cc24b0e".unsafeStrictBytes
        ),
        Vector(
          "15a5d481a00773d8976efb933abcadd186acac1365c71f07a72b4021a2845b23".unsafeStrictBytes
        )
      ),
      "41197c9607ef13853df64ac31c8378eb503af995eb829d457a87d32b11fd85b6".unsafeStrictBytes
    )

    val (sk, vk) = kesProduct.createKeyPair(specIn_seed, specIn_height, 0)
    val sk_0 = kesProduct.update(sk, 1)
    val vk_0 = kesProduct.getVerificationKey(sk_0)
    val sig_0 = kesProduct.sign(sk_0, specIn_msg)
    val sk_1 = kesProduct.update(sk, 3)
    val vk_1 = kesProduct.getVerificationKey(sk_1)
    val sig_1 = kesProduct.sign(sk_1, specIn_msg)
    val sk_2 = kesProduct.update(sk_1, 5)
    val vk_2 = kesProduct.getVerificationKey(sk_2)
    val sig_2 = kesProduct.sign(sk_2, specIn_msg)
    val sk_3 = kesProduct.update(sk_2, 7)
    val vk_3 = kesProduct.getVerificationKey(sk_3)
    val sig_3 = kesProduct.sign(sk_3, specIn_msg)

    vk shouldBe specOut_vk
    sig_0 shouldBe specOut_sig_0
    sig_1 shouldBe specOut_sig_1
    sig_2 shouldBe specOut_sig_2
    sig_3 shouldBe specOut_sig_3
    kesProduct.verify(sig_0, specIn_msg, vk_0) shouldBe true
    kesProduct.verify(sig_1, specIn_msg, vk_1) shouldBe true
    kesProduct.verify(sig_2, specIn_msg, vk_2) shouldBe true
    kesProduct.verify(sig_3, specIn_msg, vk_3) shouldBe true
  }

  it should "Test Vector - 4 - Generate and verify a specified product composition signature at t = [0, 5, 10, 15] using a provided seed, message, and heights of the two trees" in {
    val kesProduct = new KesProduct()
    val specIn_seed = "cbf5a7f7f8b807b5bc588b3c54e46fb20b680325890aa85c44ee360f99a0c483".hexStringToBytes
    val specIn_height = (2, 2)
    val specIn_time = 0
    val specIn_msg =
      "6120737472696e672070756c6c6564206f7574206f66206120776f726420656e646c6573736c792c206e6576657220736e617073".hexStringToBytes

    val specOut_vk = VerificationKeys.KesProduct(
      "e3050f622e37604a05532f3a9840de1da0bb6dfddc667b62de6dbe4baaea53e3".unsafeStrictBytes,
      specIn_time
    )
    val specOut_sig_0 = Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("2c56933618a2a11c026a937dce7bb2bab93cc98ee77a5a95dc33c20881975fb2".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "631c9ca2b3ca9154bc6abf80f612de4a45f16ec415b1d5f68d94a546805c25ff000d83f6d10ca2073bfd298a64fd815f1e64ac035a2e56d2e4763097fd7add0b".unsafeStrictBytes
        ),
        Vector(
          "8d4a0ce8ec0578634b19960288372f3580beeaeea051ee1f8dc6ae0ef2398e54".unsafeStrictBytes,
          "3e1267045aeb5ed4f3e2ff5d9d846ab36293a72907aec90e7ca05f2d7325d159".unsafeStrictBytes
        )
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("cf2ac69d0a67cf619da27e2192f0916f2f92e7164d302b5b358a01273ade173b".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "9ce5122961c4e8b58db4227ed181d4c4dbaf112a34696772115738c655ba7d95b7d381a24f35e75c5746d53de93b795ffc00f317280df8359b6f9285231b8201".unsafeStrictBytes
        ),
        Vector(
          "d5d87e3caf3dee6e6c612d7ce015055ec108bba193f022a55627ebdc4f96706f".unsafeStrictBytes,
          "7b75db1b012c029398e58cbd7e4a543dc1058772948d4124f548fe688133fc0f".unsafeStrictBytes
        )
      ),
      "0981adaabf382160a8c77c430a4d5a746f8287671604d860ebcc73fecc94343b".unsafeStrictBytes
    )

    val specOut_sig_1 = Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("282c4e4f4f44d18f50982bc725be80337cb9d4e2b44d690d55bfed5dd7ee5765".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "33bca76006fe8f19da4278bd83a92ad29b829d159f7c6feece3069567b3b372808166389e540fdb2a3f0d4b476d80c4cec82e2571cdbb4536465eab24571e30e".unsafeStrictBytes
        ),
        Vector(
          "f87bd7029be7dd9af465daec161b77971c9f46ee3e2e6e4b0019b2c2d2e13e8b".unsafeStrictBytes,
          "3e1267045aeb5ed4f3e2ff5d9d846ab36293a72907aec90e7ca05f2d7325d159".unsafeStrictBytes
        )
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("d84d2c341e6b5b55bb21091c6d53c3367376200068bad5c38f0f4ba41212ba97".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "68697afe1fa07ab861e62019ad6046cdb46fb653817d0442b0b350e4b639f12d6c27e228c5d7900381ff4dd021a4b23c89cd75d1dacbf44b0ae9cacc6c04170c".unsafeStrictBytes
        ),
        Vector(
          "a5e2626038449d723d49be411665be26bbf44b58f0acd7a9fd15f1252d674ad7".unsafeStrictBytes,
          "f7817101879a8940593bb5b0265942f2cc71996468780a30f99b270a53bcc1c2".unsafeStrictBytes
        )
      ),
      "f00c94ab9f40057e308b42de02ede3ab27499371c432c5535cc6667cc091cf02".unsafeStrictBytes
    )

    val specOut_sig_2 = Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("757c98f27cb7feb38b1b28d6e445a072ddc0a5e803cb3776779d0ea0b2962093".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "1f230333d39f0370c25fcdd2fb23942b09d2668f9d4077fde8ceee007b2a2d640ec8846e63c26c5d59f5513ad5b3ebb7992d098e539a6710fadda7a3c4585a0d".unsafeStrictBytes
        ),
        Vector(
          "0839201549e759c358618c6e5ec91612211b7d066b90a6d8fc82b315dbc42bcd".unsafeStrictBytes,
          "5bd9f2be21473982e1a8bf7af92948532de9ffecee7a615ac9d404388d8b59b9".unsafeStrictBytes
        )
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("d1957ba36686a69f47e176a352128cc6281c9bfe4712a935df11994e4d9eec1d".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "e1ac53ccb5a1dde07c17240f150f753ce19141d1ba805dc1ca886fa7c421a4871b25ec71f516f603eb85d5c8335fae4e1e3b243589616da06b8635d8ef79620b".unsafeStrictBytes
        ),
        Vector(
          "bfc3bc84429f922563e73fd72e20797e15806c749a17a4bbf3648c9d4ef000a7".unsafeStrictBytes,
          "476821ed21d5f1ed7371e99a203700b308bd0a8fd2a2742e8f38ad62489c5dcb".unsafeStrictBytes
        )
      ),
      "f78f562cc3687e10358f67b713aa5e025d96e29cab4e53a521364b50938b146c".unsafeStrictBytes
    )

    val specOut_sig_3 = Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("df15213e0dc7fc0d4119922acc2f3f8cf5772615534bb30944e330a00f43778b".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "524c8eaa6add002e6e13d138d0fc8e8ebb9e85c7ac502bb43502632761e5521eff78b7360b42e21ccd25e46ea6ca59fda4ed521e9b2f851ea1450c8acdae270b".unsafeStrictBytes
        ),
        Vector(
          "2fea432763055da30976aefc5a71ce7cb629e0b2decda4add244852e2c7c0009".unsafeStrictBytes,
          "5bd9f2be21473982e1a8bf7af92948532de9ffecee7a615ac9d404388d8b59b9".unsafeStrictBytes
        )
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("a2b49b0bc62ab98c64e965bca61c02c91923d651bb65ce92a814523445889888".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "09e6365035e00a650e66893da09a6e32778ef4129f8541c7bffc1448ab91cfd204f65c8078ef7aea769b3ef1dae6979d3988f904df7b1c2822b76739cf76f00f".unsafeStrictBytes
        ),
        Vector(
          "5899b296c3fd5a15f9e00d54bf8bbd12329d4db38d587d0ac5e3038e298fbabe".unsafeStrictBytes,
          "097a120b5535bb352bf964fd20b741116d70f5eb34c0a2bb8883888ab701516b".unsafeStrictBytes
        )
      ),
      "6437dc07114792cd1009b46fd7b100032c7991c456875c133533069308ea2894".unsafeStrictBytes
    )

    val (sk, vk) = kesProduct.createKeyPair(specIn_seed, specIn_height, 0)
    val sig_0 = kesProduct.sign(sk, specIn_msg)
    val sk_1 = kesProduct.update(sk, 5)
    val vk_1 = kesProduct.getVerificationKey(sk_1)
    val sig_1 = kesProduct.sign(sk_1, specIn_msg)
    val sk_2 = kesProduct.update(sk_1, 10)
    val vk_2 = kesProduct.getVerificationKey(sk_2)
    val sig_2 = kesProduct.sign(sk_2, specIn_msg)
    val sk_3 = kesProduct.update(sk_2, 15)
    val vk_3 = kesProduct.getVerificationKey(sk_3)
    val sig_3 = kesProduct.sign(sk_3, specIn_msg)

    vk shouldBe specOut_vk
    sig_0 shouldBe specOut_sig_0
    sig_1 shouldBe specOut_sig_1
    sig_2 shouldBe specOut_sig_2
    sig_3 shouldBe specOut_sig_3
    kesProduct.verify(sig_0, specIn_msg, vk) shouldBe true
    kesProduct.verify(sig_1, specIn_msg, vk_1) shouldBe true
    kesProduct.verify(sig_2, specIn_msg, vk_2) shouldBe true
    kesProduct.verify(sig_3, specIn_msg, vk_3) shouldBe true
  }

  it should "Test Vector - 5 - Generate and verify a specified product composition signature at t = [0, 21, 42, 63] using a provided seed, message, and heights of the two trees" in {
    val kesProduct = new KesProduct()
    val specIn_seed = "50c3e574fa16956b384f9c46ecf976a0eb42fd82b9e8be381d5a24b3697d9e6d".hexStringToBytes
    val specIn_height = (3, 3)
    val specIn_time = 0
    val specIn_msg =
      "64697361626c656420616e642064657461696e65642c2070726f6261626c792064697370656c6c6564".hexStringToBytes

    val specOut_vk = VerificationKeys.KesProduct(
      "e290c6600ce59025b705362b725c522d52c749167516213a73272e6861ef5dce".unsafeStrictBytes,
      specIn_time
    )
    val specOut_sig_0 = Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("11b542df6036003b84c4f6af7bcc6bf4890a92fb503e4c44344703a3d9a56616".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "1f72de09d38cffb4aee83fa4b6ff3e0b2b11bd0072fa83ebeac8dbcad5a6132a3017fbeba19bf9e83a6882ef4af80d564da9ff412d4686051b17351ec4c8a200".unsafeStrictBytes
        ),
        Vector(
          "7557bdc2b80dacdfc6f50ff7153c0121f6bc0928fa03a25016d6f29f075bb41e".unsafeStrictBytes,
          "cfeb55b068dc7842e4a9b5bfbab5a2f12ba803b1f26f6e58d45fda790b91c910".unsafeStrictBytes,
          "3326b87fe172fea589f79de8ba7bea0b0f288ede17c3c2433dd2cc9c5ec01570".unsafeStrictBytes
        )
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("643119fe671768ecac4a7fa999a7b6719d02eea1de127f9564c68566702a96be".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "63ff2586a56c909309a5c71d4bb4a738920381a516411bda045b02f78b04f670189df367d781ed491d195571a63e1a4baadf34a879a02a1c3fb00dcaf7603e0c".unsafeStrictBytes
        ),
        Vector(
          "51435e6993cdf9fcddc7da914e7cea2076e64f09037e5609f2a4711b814861fe".unsafeStrictBytes,
          "07ded1379fcda41b5214a607487616e22c863fd86ba17b85943a608046f43761".unsafeStrictBytes,
          "a6aab8f3954f8adafc89df1e9ed6f0b160dda3d73a6a34cae4566eb3161d7d95".unsafeStrictBytes
        )
      ),
      "1766bf6233df5efb27cb9a626477668a93479ebd84fd55fb7041a7a29c93d6c2".unsafeStrictBytes
    )

    val specOut_sig_1 = Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("a006b5e71a16e51781c4185ceb738914b21bcb38544ecbe63407ba5bc888541c".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "1f76f27fbc6953151309e7b09d4d4c691ff7a8d4e1cfac164588942573107086f37739b3db1f06d4c629d1d6154e0d5cf352abb27be3a98b66c810cbda744c07".unsafeStrictBytes
        ),
        Vector(
          "c7521a93e407b29a24ec8df7c3cbc07352ed3f1e2f684933c0ba6b5322162eff".unsafeStrictBytes,
          "4abc0c96474b57f6d82c74a11a88112d20160392b837172bed957c9c4052d0a1".unsafeStrictBytes,
          "3326b87fe172fea589f79de8ba7bea0b0f288ede17c3c2433dd2cc9c5ec01570".unsafeStrictBytes
        )
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("66faac397e9563745f2623a1825c90366cc7971d0b1a0c87f3722685caae6a2d".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "ae143f5c4b255b1448ec72263d9e6a4b5fea972501adafb2351daddad84dd32a299fb5519e290b57ea15ae9c62fc6494de8cd2f2bfff0989baffb4866566a10b".unsafeStrictBytes
        ),
        Vector(
          "fc6dd766522c4be77222ce112588f731e17ea37b61f3b5b908dab761c6676bd9".unsafeStrictBytes,
          "3146459cc09e66e9a32e251e620c8f264581134bbcd8a81d3aa94e2017d02158".unsafeStrictBytes,
          "1245106090fa427604bbecaab61d9c157c8a1462955ff9986058845cc99c7311".unsafeStrictBytes
        )
      ),
      "9cc2d6121dc44f6afb2d1d4d2163af8c8262d6c93770f33e947facada5906fa9".unsafeStrictBytes
    )

    val specOut_sig_2 = Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("a3be6fd324fcb51afe38c6ce4cf97273b06a4836896da1868234f823c4b24477".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "c59c276e385b036d7e3911fb957f2db972bf8010f1ba09e52f7cf9d20dda03c801adc5918d73b1c05763f6abe0a2503ce7c67870f0dad891d199ee5f33e6b707".unsafeStrictBytes
        ),
        Vector(
          "fe82b54e078c986aa9c25a0e914463c8b8361692221c8a2716c6dc3d746fd08b".unsafeStrictBytes,
          "b02c889de52f5ce59b7778a771631a0ffa4b99419c4a2078102be8a4465aac6e".unsafeStrictBytes,
          "33d94f87778d5204ceb3b8e3b3768600627a1b6ad66d078e1a3541bd4fcda649".unsafeStrictBytes
        )
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("9a33868bad6a3713b54962d42e4a17b135845281cdf8786ac100865e2ceb6d14".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "376d8cfea55f0275f24b02e020222be2170aec07cfb1249855ca7d0ffa35393b6c0bb977cc167095c8f4de3574b773c278e888b4cc2b1c4e9da8b6fa7ec92303".unsafeStrictBytes
        ),
        Vector(
          "bcbecf84e3bc0f4d482dee450e19efbe579e3f1121e7e8a24a746c416e327e97".unsafeStrictBytes,
          "3044d1d9a57616e8ed109dc37a585d5add07dddd4a0ff0e2e110deb162c5bc36".unsafeStrictBytes,
          "7c414edab64a076d47ce39d28bc3b76952e0c552f1b4baaf6460e56d00429019".unsafeStrictBytes
        )
      ),
      "e1b233d3e9969137ce9d7684316135ef76a2fe507692cf1f1528f6abfc756892".unsafeStrictBytes
    )

    val specOut_sig_3 = Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("84e857556ab0f306d9ccaf07951f0b9a4326d8f470d2761465f076cf9afc3bda".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "2b981e6768d0e2bb83cf7b640e34861fd57f47e018c610bef691a1a7a6529a5505bb4ac7f598f057bd91c0d69265077c8d5da3ed35e1964af4bda4907ea66b0b".unsafeStrictBytes
        ),
        Vector(
          "85a8aed9dc95730edf357a850ab80090c142b93058dd0b8e5f3cd3319a60ab9f".unsafeStrictBytes,
          "2e15e54671daf8576d5b2d9d38708b2d2601a038192018984d19f11bf72702e4".unsafeStrictBytes,
          "33d94f87778d5204ceb3b8e3b3768600627a1b6ad66d078e1a3541bd4fcda649".unsafeStrictBytes
        )
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519("9ebeebb407842ae0f67d9aee5ffabc32a8e6a1bd7810c4da995a578529565e29".unsafeStrictBytes),
        Proofs.Knowledge.Ed25519(
          "07329c9802f10e01783fa5ed9081673098effdcca4ee45d6fb5788c1129cb8f0f7370e9078e8c58aed2aea4de95e72ec7a213d74104739c415dac77cb47e2501".unsafeStrictBytes
        ),
        Vector(
          "05d621aa665d76e25c5f535a5a15b09e46c4aabc744de1801c2749f9f52a95c0".unsafeStrictBytes,
          "bc0225bcc28bed4ccc43c3a4910c2ad3e2d62894ebdb9ba67190df085858934d".unsafeStrictBytes,
          "f18710ea9ac9e6ca20070beddbcd7ed0b1608aa10116a98a197b8639cdbcc337".unsafeStrictBytes
        )
      ),
      "1b9972beb1238eee1f032c48a026244e407f19d63c34baef8348ac996fcac9d7".unsafeStrictBytes
    )

    val (sk, vk) = kesProduct.createKeyPair(specIn_seed, specIn_height, 0)
    val sig_0 = kesProduct.sign(sk, specIn_msg)
    val sk_1 = kesProduct.update(sk, 21)
    val vk_1 = kesProduct.getVerificationKey(sk_1)
    val sig_1 = kesProduct.sign(sk_1, specIn_msg)
    val sk_2 = kesProduct.update(sk_1, 42)
    val vk_2 = kesProduct.getVerificationKey(sk_2)
    val sig_2 = kesProduct.sign(sk_2, specIn_msg)
    val sk_3 = kesProduct.update(sk_2, 63)
    val vk_3 = kesProduct.getVerificationKey(sk_3)
    val sig_3 = kesProduct.sign(sk_3, specIn_msg)

    vk shouldBe specOut_vk
    sig_0 shouldBe specOut_sig_0
    sig_1 shouldBe specOut_sig_1
    sig_2 shouldBe specOut_sig_2
    sig_3 shouldBe specOut_sig_3
    kesProduct.verify(sig_0, specIn_msg, vk) shouldBe true
    kesProduct.verify(sig_1, specIn_msg, vk_1) shouldBe true
    kesProduct.verify(sig_2, specIn_msg, vk_2) shouldBe true
    kesProduct.verify(sig_3, specIn_msg, vk_3) shouldBe true
  }

}
