package co.topl.crypto.mnemonic

import co.topl.crypto.mnemonic.FromEntropy.Instances._
import co.topl.crypto.mnemonic.FromEntropy.derive
import co.topl.crypto.mnemonic.Language._
import co.topl.crypto.mnemonic.MnemonicSize._
import co.topl.crypto.utils.Generators._
import co.topl.crypto.utils.Hex.implicits._
import co.topl.models.SecretKeys
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Lengths, Sized}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class MnemonicToExtendedEd25519
    extends AnyFlatSpec
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with EitherValues {

  case class TestVector(name: String, specIn: SpecIn, specOut: SpecOut)
  case class SpecIn(phrase: String, size: MnemonicSize, language: Language, password: String)
  case class SpecOut(sk: SecretKeys.ExtendedEd25519)

  object SpecOut {

    def fromHexString(hexString: String): SpecOut = {
      val allBytes = hexString.unsafeStrictBytes[Lengths.`96`.type].data
      SpecOut(
        SecretKeys.ExtendedEd25519(
          Sized.strictUnsafe(allBytes.slice(0, 32)),
          Sized.strictUnsafe(allBytes.slice(32, 64)),
          Sized.strictUnsafe(allBytes.slice(64, 96))
        )
      )
    }
  }

  "Key from mnemonic phrase" should "output same the key with the same password" in {
    val createKey: String => SecretKeys.ExtendedEd25519 =
      derive[String => SecretKeys.ExtendedEd25519](
        "ozone drill grab fiber curtain grace pudding thank cruise elder eight picnic",
        Mnemonic12,
        English
      ).value

    forAll(stringGen) { password =>
      val firstAttempt = createKey(password)
      val secondAttempt = createKey(password)

      firstAttempt.leftKey shouldBe secondAttempt.leftKey
      firstAttempt.rightKey shouldBe secondAttempt.rightKey
      firstAttempt.chainCode shouldBe secondAttempt.chainCode
    }
  }
  it should "output a different key with a different password" in {
    val createKey: String => SecretKeys.ExtendedEd25519 =
      derive[String => SecretKeys.ExtendedEd25519](
        "ozone drill grab fiber curtain grace pudding thank cruise elder eight picnic",
        Mnemonic12,
        English
      ).value

    forAll(stringGen, stringGen) { (password1, password2) =>
      val firstAttempt = createKey(password1)
      val secondAttempt = createKey(password2)

      if (password1 != password2) {
        firstAttempt.leftKey should not be secondAttempt.leftKey
        firstAttempt.rightKey should not be secondAttempt.rightKey
        firstAttempt.chainCode should not be secondAttempt.chainCode
      }
    }
  }
  it should "satisfy test vector 1" in {
    val tv = TestVector(
      "Test Vector #1",
      SpecIn(
        "buyer bomb chapter carbon chair grid wheel protect giraffe spike pupil model",
        Mnemonic12,
        English,
        "dinner"
      ),
      SpecOut.fromHexString(
        "10d6d266fc36cce2ee95197c968983b1b8a0cf26030068f0aa6d7603515d2749ff030cd54551eaeef0b2d22305d6984c1313b" +
        "855775f6bfb9fc4aff1a8aa837e43773dc6ead8b276897e03687a943ffa795f35c4dfc438f307106509ba96bf36"
      )
    )

    val sk = FromEntropy
      .derive[String => SecretKeys.ExtendedEd25519](tv.specIn.phrase, tv.specIn.size, tv.specIn.language)
      .map(_.apply(tv.specIn.password))
      .value

    sk shouldBe tv.specOut.sk
  }
  it should "satisfy test vector 2" in {
    val tv = TestVector(
      "Test Vector #2",
      SpecIn(
        "vessel erase town arrow girl emotion siren better fork approve spare convince sauce amused clap",
        Mnemonic15,
        English,
        "heart"
      ),
      SpecOut.fromHexString(
        "105c0659a289eb1899ad891da6022155729fe7b4cd59399cf82abd5df6e7024714a4cfd8efde026641d43fb4945dbc1d83934" +
        "fbea856264f1198a4f9e34fc79dadd5982a6cb2a9ad628f4197945ab8170071af566c188c4ee35ee589a7792fed"
      )
    )

    val sk = FromEntropy
      .derive[String => SecretKeys.ExtendedEd25519](tv.specIn.phrase, tv.specIn.size, tv.specIn.language)
      .map(_.apply(tv.specIn.password))
      .value

    sk shouldBe tv.specOut.sk
  }
  it should "satisfy test vector 3" in {
    val tv = TestVector(
      "Test Vector #3",
      SpecIn(
        "model abandon genius figure shiver craft surround sister permit output network swift slush lumber " +
        "dune license run sugar",
        Mnemonic18,
        English,
        "describe"
      ),
      SpecOut.fromHexString(
        "f8018c7179445139ab5b437e66c6109bed6879dd603a278bccec3bc6a020c25ed10a9a14b6351f0a4b68b164ada673f731738a9" +
        "62da627b399d7c5cd2fb3616a67d34d5baaf63a7c5a8159dec79de21a937a03baeb05548c91d7e2c5d648cf4e"
      )
    )

    val sk = FromEntropy
      .derive[String => SecretKeys.ExtendedEd25519](tv.specIn.phrase, tv.specIn.size, tv.specIn.language)
      .map(_.apply(tv.specIn.password))
      .value

    sk shouldBe tv.specOut.sk
  }
  it should "satisfy test vector 4" in {
    val tv = TestVector(
      "Test Vector #4",
      SpecIn(
        "acquire pretty ocean screen assist purity exchange memory universe attitude sense charge fragile emerge " +
        "quick home asthma intact gloom giant gather",
        Mnemonic21,
        English,
        "manager"
      ),
      SpecOut.fromHexString(
        "90e7716fce3ee6b32c3a8c89d54cb01a63596c9a19c270cfcfdff29b9c585555beba8d3d4113558a3411267c454b2215fc5f1bb" +
        "429d7751f051a88942d9e3c7a8c61503b5b06f56bd3873a2fab636d87e064b3b3dca5a329646dedaab1e02c05"
      )
    )

    val sk = FromEntropy
      .derive[String => SecretKeys.ExtendedEd25519](tv.specIn.phrase, tv.specIn.size, tv.specIn.language)
      .map(_.apply(tv.specIn.password))
      .value

    sk shouldBe tv.specOut.sk
  }
  it should "satisfy test vector 5" in {
    val tv = TestVector(
      "Test Vector #5",
      SpecIn(
        "nice demise viable bonus flavor genre kick nominee supreme couple tattoo shadow ethics swamp rebuild pencil " +
        "rebuild pet ignore define seek fire wrong harvest",
        Mnemonic24,
        English,
        "exact"
      ),
      SpecOut.fromHexString(
        "98df2e07e25a733a6d2a2636b2bd67408687fb1da399abc164ed258a98b9655f1997507da125e2d77a0c2f168a866ea8fe9a7c0e27fb" +
        "772b287a702d9742fb9fe14b0893136596df5de01ec9b6487a865bd415cb8a6ca96eb582e81777802461"
      )
    )

    val sk = FromEntropy
      .derive[String => SecretKeys.ExtendedEd25519](tv.specIn.phrase, tv.specIn.size, tv.specIn.language)
      .map(_.apply(tv.specIn.password))
      .value

    sk shouldBe tv.specOut.sk
  }
  it should "satisfy test vector 6" in {
    val tv = TestVector(
      "Test Vector #6",
      SpecIn(
        "toss enrich steak utility dolphin cushion jeans work ski angle total alley trade poem february whisper toe " +
        "office half assume keep shift also fade",
        Mnemonic24,
        English,
        ""
      ),
      SpecOut.fromHexString(
        "686657f893d3d2c14bc3ab93d693bfbc868a621790d8aca64317834ed3aa85537d8d094a432cb852bb3b8617aeab621c0459b84a6d24" +
        "80735b4e3ff8bff11657944aaad3c8b7633eafb5871de4122241728ce5d5edd8268472c7c980252fc55b"
      )
    )

    val sk = FromEntropy
      .derive[String => SecretKeys.ExtendedEd25519](tv.specIn.phrase, tv.specIn.size, tv.specIn.language)
      .map(_.apply(tv.specIn.password))
      .value

    sk shouldBe tv.specOut.sk
  }
  // https://github.com/cardano-foundation/CIPs/blob/master/CIP-0003/Icarus.md#test-vectors
  it should "satisfy test vector 1 from icarus" in {
    val tv = TestVector(
      "Icarus Test Vector #1",
      SpecIn(
        "eight country switch draw meat scout mystery blade tip drift useless good keep usage title",
        Mnemonic15,
        English,
        ""
      ),
      SpecOut.fromHexString(
        "c065afd2832cd8b087c4d9ab7011f481ee1e0721e78ea5dd609f3ab3f156d245d176bd8fd4ec60b4731c3918a2a72a0" +
        "226c0cd119ec35b47e4d55884667f552a23f7fdcd4a10c6cd2c7393ac61d877873e248f417634aa3d812af327ffe9d620"
      )
    )

    val sk = FromEntropy
      .derive[String => SecretKeys.ExtendedEd25519](tv.specIn.phrase, tv.specIn.size, tv.specIn.language)
      .map(_.apply(tv.specIn.password))
      .value

    sk shouldBe tv.specOut.sk
  }
  // https://github.com/cardano-foundation/CIPs/blob/master/CIP-0003/Icarus.md#test-vectors
  it should "satisfy test vector 2 from icarus" in {
    val tv = TestVector(
      "Icarus Test Vector #1",
      SpecIn(
        "eight country switch draw meat scout mystery blade tip drift useless good keep usage title",
        Mnemonic15,
        English,
        "foo"
      ),
      SpecOut.fromHexString(
        "70531039904019351e1afb361cd1b312a4d0565d4ff9f8062d38acf4b15cce41d7b5738d9c893feea55512a3004acb0" +
        "d222c35d3e3d5cde943a15a9824cbac59443cf67e589614076ba01e354b1a432e0e6db3b59e37fc56b5fb0222970a010e"
      )
    )

    val sk = FromEntropy
      .derive[String => SecretKeys.ExtendedEd25519](tv.specIn.phrase, tv.specIn.size, tv.specIn.language)
      .map(_.apply(tv.specIn.password))
      .value

    sk shouldBe tv.specOut.sk
  }
}
