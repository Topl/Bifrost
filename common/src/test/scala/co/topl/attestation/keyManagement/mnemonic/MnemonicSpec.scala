package co.topl.attestation.keyManagement.mnemonic

import co.topl.attestation.keyManagement.derivedKeys.{DerivedKeyIndex, ExtendedPrivateKeyEd25519, HardenedIndex}
import co.topl.attestation.keyManagement.derivedKeys.ExtendedPrivateKeyEd25519.Password
import co.topl.attestation.keyManagement.derivedKeys.implicits._
import co.topl.attestation.keyManagement.mnemonic.Language._
import co.topl.attestation.keyManagement.mnemonic.MnemonicSize._
import co.topl.utils.CommonGenerators
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.SizedBytes.implicits._
import co.topl.utils.StringDataTypes.Base16Data
import co.topl.utils.codecs.implicits._
import co.topl.utils.encode.Base58
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import co.topl.utils.StringDataTypes.implicits._
import scodec.bits.ByteVector

class MnemonicSpec
    extends AnyPropSpec
    with CommonGenerators
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks {

  implicit val entropyAsString: FromEntropy[String] =
    (e: Entropy) => Base58.encode(e.value)

  property("12 phrase mnemonic with valid words should be valid") {
    val phrase = "cat swing flag economy stadium alone churn speed unique patch report train"
    val mnemonic = derive[String](phrase, Mnemonic12, English)

    mnemonic.isRight shouldBe true
  }

  property("12 phrase mnemonic with invalid word length should be invalid") {
    val phrase = "result fresh margin life life filter vapor trim"

    val mnemonic = derive[String](phrase, Mnemonic12, English)

    mnemonic.isLeft shouldBe true
  }

  property("12 phrase mnemonic with invalid words should be invalid") {
    val phrase = "amber glue hallway can truth drawer wave flex cousin grace close compose"

    val mnemonic = derive[String](phrase, Mnemonic12, English)

    mnemonic.isLeft shouldBe true
  }

  property("12 phrase mnemonic with valid words and invalid checksum should be invalid") {
    val phrase = "ugly wire busy skate slice kidney razor eager bicycle struggle aerobic picnic"
    val mnemonic = derive[String](phrase, Mnemonic12, English)

    mnemonic.isLeft shouldBe true
  }

  def entropyLengthTest(bytes: Int, size: MnemonicSize): Unit =
    property(s"from entropy of length $bytes should be valid") {
      forAll(specificLengthBytesGen(bytes)) { entropy =>
        if (entropy.length == bytes) {
          val entropyString = derive[String](entropy, size)

          entropyString.isRight shouldBe true
        }
      }
    }

  entropyLengthTest(16, Mnemonic12)
  entropyLengthTest(20, Mnemonic15)
  entropyLengthTest(24, Mnemonic18)
  entropyLengthTest(28, Mnemonic21)
  entropyLengthTest(32, Mnemonic24)

  property("mnemonic with extra whitespace is valid") {
    val mnemonic =
      derive[String](
        "vessel ladder alter error  federal sibling chat   ability sun glass valve picture",
        Mnemonic12,
        English
      )

    mnemonic.isRight shouldBe true
  }

  property("mnemonic with extra whitespace has same value as single spaced") {
    val expected =
      derive[String](
        "vessel ladder alter error federal sibling chat ability sun glass valve picture",
        Mnemonic12,
        English
      ).getOrThrow()

    val result =
      derive[String](
        "vessel ladder alter error  federal sibling chat   ability sun glass valve picture",
        Mnemonic12,
        English
      ).getOrThrow()

    result shouldBe expected
  }

  property("mnemonic with capital letters is valid") {
    val mnemonic = derive[String](
      "Legal Winner Thank Year Wave Sausage Worth Useful Legal " +
      "Winner Thank Year Wave Sausage Worth Useful Legal Will",
      Mnemonic18,
      English
    )

    mnemonic.isRight shouldBe true
  }

  property("mnemonic with capital letters has same entropy as lowercase") {
    val expectedEntropy =
      derive[String](
        "legal winner thank year wave sausage worth useful legal " +
        "winner thank year wave sausage worth useful legal will",
        Mnemonic18,
        English
      )
        .getOrThrow()

    val result =
      derive[String](
        "Legal Winner Thank Year Wave Sausage Worth Useful Legal " +
        "Winner Thank Year Wave Sausage Worth Useful Legal Will",
        Mnemonic18,
        English
      )
        .getOrThrow()

    result shouldBe expectedEntropy
  }

  property("mnemonic with unusual characters is invalid") {
    val entropy =
      derive[String](
        "voi\uD83D\uDD25d come effort suffer camp su\uD83D\uDD25rvey warrior heavy shoot primary" +
        " clutch c\uD83D\uDD25rush" +
        " open amazing screen " +
        "patrol group space point ten exist slush inv\uD83D\uDD25olve unfold",
        Mnemonic24,
        English
      )

    entropy.isLeft shouldBe true
  }

  case class TestVector(
    name:       String,
    phrase:     String,
    size:       MnemonicSize,
    language:   Language,
    password:   String,
    privateKey: String
  )

  // Test Vectors
  // https://topl.atlassian.net/wiki/spaces/Bifrost/pages/294813812/HD+Wallet+Protocols+and+Test+Vectors
  val testVectors = Seq(
    TestVector(
      "Test Vector #1",
      "buyer bomb chapter carbon chair grid wheel protect giraffe spike pupil model",
      Mnemonic12,
      English,
      "dinner",
      "10d6d266fc36cce2ee95197c968983b1b8a0cf26030068f0aa6d7603515d2749ff030cd54551eaeef0b2d22305d6984c1313b" +
      "855775f6bfb9fc4aff1a8aa837e43773dc6ead8b276897e03687a943ffa795f35c4dfc438f307106509ba96bf36"
    ),
    TestVector(
      "Test Vector #2",
      "vessel erase town arrow girl emotion siren better fork approve spare convince sauce amused clap",
      Mnemonic15,
      English,
      "heart",
      "105c0659a289eb1899ad891da6022155729fe7b4cd59399cf82abd5df6e7024714a4cfd8efde026641d43fb4945dbc1d83934" +
      "fbea856264f1198a4f9e34fc79dadd5982a6cb2a9ad628f4197945ab8170071af566c188c4ee35ee589a7792fed"
    ),
    TestVector(
      "Test Vector #3",
      "model abandon genius figure shiver craft surround sister permit output network swift slush lumber " +
      "dune license run sugar",
      Mnemonic18,
      English,
      "describe",
      "f8018c7179445139ab5b437e66c6109bed6879dd603a278bccec3bc6a020c25ed10a9a14b6351f0a4b68b164ada673f731738a9" +
      "62da627b399d7c5cd2fb3616a67d34d5baaf63a7c5a8159dec79de21a937a03baeb05548c91d7e2c5d648cf4e"
    ),
    TestVector(
      "Test Vector #4",
      "acquire pretty ocean screen assist purity exchange memory universe attitude sense charge fragile emerge " +
      "quick home asthma intact gloom giant gather",
      Mnemonic21,
      English,
      "manager",
      "90e7716fce3ee6b32c3a8c89d54cb01a63596c9a19c270cfcfdff29b9c585555beba8d3d4113558a3411267c454b2215fc5f1bb" +
      "429d7751f051a88942d9e3c7a8c61503b5b06f56bd3873a2fab636d87e064b3b3dca5a329646dedaab1e02c05"
    ),
    TestVector(
      "Test Vector #5",
      "nice demise viable bonus flavor genre kick nominee supreme couple tattoo shadow ethics swamp rebuild pencil " +
      "rebuild pet ignore define seek fire wrong harvest",
      Mnemonic24,
      English,
      "exact",
      "98df2e07e25a733a6d2a2636b2bd67408687fb1da399abc164ed258a98b9655f1997507da125e2d77a0c2f168a866ea8fe9a7c0e27fb" +
      "772b287a702d9742fb9fe14b0893136596df5de01ec9b6487a865bd415cb8a6ca96eb582e81777802461"
    ),
    TestVector(
      "Test Vector #6",
      "toss enrich steak utility dolphin cushion jeans work ski angle total alley trade poem february whisper toe " +
      "office half assume keep shift also fade",
      Mnemonic24,
      English,
      "",
      "686657f893d3d2c14bc3ab93d693bfbc868a621790d8aca64317834ed3aa85537d8d094a432cb852bb3b8617aeab621c0459b84a6d24" +
      "80735b4e3ff8bff11657944aaad3c8b7633eafb5871de4122241728ce5d5edd8268472c7c980252fc55b"
    )
  )

  def testVectorTest(tv: TestVector): Unit =
    property(s"Entropy Test: ${tv.name}") {
      val expectedPrivateKeyBase16 = Base16Data.unsafe(tv.privateKey)

      val pkResult =
        derive[Password => ExtendedPrivateKeyEd25519](tv.phrase, tv.size, tv.language).getOrThrow()(tv.password)

      val pkResultBase16 =
        (pkResult.leftKey.toVector ++ pkResult.rightKey.toVector ++ pkResult.chainCode.toVector).encodeAsBase16

      pkResultBase16 shouldBe expectedPrivateKeyBase16
    }

  testVectors.foreach(testVectorTest)

  // test cloned from
  // https://topl.atlassian.net/wiki/spaces/CORE/pages/304023492/HD+Wallet+Protocols+and+Test+Vectors#HD-Wallet-End-to-End-Testing-(Including-Topl-Path-Adoption)
  property("end-to-end wallet test") {
    val mnemonic =
      "rude stadium move tumble spice vocal undo butter cargo win valid session question walk indoor nothing wagon " +
      "column artefact monster fold gallery receive just"

    val expectedEntropy = Base16Data.unsafe("bcfa7e43752d19eabb38fa22bf6bc3622af9ed1cc4b6f645b833c7a5a8be2ce3")

    val expectedRootSecretKey = Array(112, 117, 59, 231, 105, 163, 101, 242, 141, 62, 216, 196, 229, 115, 212, 55, 8,
      164, 41, 112, 217, 8, 6, 251, 158, 139, 43, 80, 44, 233, 169, 76, 14, 67, 79, 200, 233, 248, 142, 49, 252, 139,
      11, 221, 128, 34, 58, 200, 254, 55, 38, 149, 151, 73, 95, 240, 100, 125, 37, 101, 155, 144, 5, 13, 28, 50, 236,
      47, 75, 90, 232, 36, 147, 188, 217, 198, 50, 22, 196, 254, 142, 105, 205, 195, 57, 160, 171, 74, 184, 12, 58, 141,
      143, 157, 230, 227).map(_.toByte).encodeAsBase16

    val expectedRootPublicKey = Array(26, 26, 164, 138, 49, 3, 178, 120, 177, 164, 84, 13, 134, 243, 172, 53, 49, 77,
      108, 251, 84, 85, 145, 17, 150, 1, 159, 238, 4, 68, 187, 137, 28, 50, 236, 47, 75, 90, 232, 36, 147, 188, 217,
      198, 50, 22, 196, 254, 142, 105, 205, 195, 57, 160, 171, 74, 184, 12, 58, 141, 143, 157, 230, 227)
      .map(_.toByte)
      .encodeAsBase16

    val `expected m/1852' secret` = Array(104, 249, 139, 157, 160, 184, 148, 188, 66, 30, 151, 80, 233, 196, 219, 210,
      24, 198, 7, 128, 207, 111, 155, 19, 77, 69, 172, 30, 48, 233, 169, 76, 49, 66, 7, 169, 54, 168, 67, 241, 70, 119,
      139, 237, 145, 23, 32, 209, 35, 24, 190, 179, 74, 205, 75, 201, 189, 158, 43, 174, 140, 50, 29, 228, 89, 149, 165,
      10, 10, 2, 214, 69, 109, 153, 181, 40, 100, 52, 43, 27, 239, 74, 243, 207, 145, 162, 81, 99, 77, 92, 210, 97, 213,
      191, 25, 6).map(_.toByte).encodeAsBase16

    val `expected m/1852'/7091' secret` = Array(232, 208, 229, 138, 24, 98, 41, 4, 64, 168, 156, 114, 212, 141, 50, 222,
      173, 126, 237, 212, 50, 16, 83, 131, 28, 13, 170, 105, 51, 233, 169, 76, 47, 236, 83, 116, 215, 62, 234, 46, 12,
      41, 134, 60, 197, 94, 74, 66, 67, 244, 169, 136, 18, 137, 169, 8, 29, 227, 53, 153, 23, 155, 138, 249, 181, 18,
      158, 93, 62, 68, 21, 37, 81, 13, 196, 112, 234, 63, 245, 222, 232, 70, 128, 20, 225, 172, 184, 103, 46, 110, 0,
      243, 204, 218, 7, 19).map(_.toByte).encodeAsBase16

    val `expected m/1852'/7091'/0' secret` = Array(144, 192, 89, 83, 27, 227, 138, 16, 154, 2, 137, 204, 189, 197, 149,
      28, 27, 150, 134, 47, 236, 17, 108, 141, 38, 41, 82, 85, 52, 233, 169, 76, 244, 14, 3, 40, 42, 104, 115, 32, 124,
      122, 15, 210, 193, 166, 191, 183, 171, 77, 3, 142, 160, 100, 195, 69, 169, 33, 117, 95, 7, 109, 173, 110, 254, 31,
      135, 36, 175, 128, 254, 152, 162, 52, 116, 234, 79, 254, 242, 114, 118, 80, 25, 248, 88, 241, 50, 190, 3, 242,
      142, 80, 120, 47, 63, 193).map(_.toByte).encodeAsBase16

    val `expected m/1852'/7091'/0'/0 secret` = Array(224, 65, 252, 48, 76, 184, 7, 234, 7, 234, 75, 22, 246, 12, 36, 11,
      0, 122, 36, 201, 83, 33, 144, 33, 14, 64, 250, 121, 56, 233, 169, 76, 0, 255, 140, 17, 176, 35, 77, 236, 199, 161,
      249, 54, 76, 43, 110, 230, 249, 229, 231, 131, 226, 165, 92, 42, 168, 144, 179, 247, 156, 254, 154, 33, 9, 190,
      208, 81, 50, 102, 249, 40, 172, 164, 53, 183, 243, 98, 49, 251, 176, 141, 158, 4, 29, 18, 226, 166, 142, 14, 91,
      120, 28, 171, 190, 82).map(_.toByte).encodeAsBase16

    val `expected m/1852'/7091'/0'/0/0 secret` = Array(168, 233, 30, 34, 115, 135, 88, 135, 12, 240, 194, 63, 243, 146,
      150, 112, 3, 232, 67, 71, 100, 137, 172, 24, 220, 199, 252, 222, 56, 233, 169, 76, 142, 116, 48, 51, 157, 72, 93,
      184, 101, 78, 244, 126, 43, 158, 173, 201, 59, 56, 49, 183, 133, 88, 148, 101, 149, 232, 114, 175, 231, 181, 208,
      22, 101, 238, 109, 12, 56, 7, 44, 60, 117, 15, 27, 109, 203, 123, 1, 142, 93, 43, 157, 64, 216, 73, 85, 137, 144,
      67, 21, 130, 141, 64, 51, 38).map(_.toByte).encodeAsBase16

    val `expected m/1852'/7091'/0'/0/0 verify` = Array(198, 105, 157, 143, 239, 12, 101, 203, 178, 52, 197, 44, 213, 82,
      65, 46, 92, 139, 69, 96, 158, 93, 187, 137, 124, 251, 19, 209, 17, 0, 60, 48, 101, 238, 109, 12, 56, 7, 44, 60,
      117, 15, 27, 109, 203, 123, 1, 142, 93, 43, 157, 64, 216, 73, 85, 137, 144, 67, 21, 130, 141, 64, 51, 38)
      .map(_.toByte)
      .encodeAsBase16

    val languageWordList = LanguageWordList.validated(English).getOrThrow()

    val mnemonicPhrase = Phrase.validated(mnemonic, Mnemonic24, languageWordList).getOrThrow()

    val entropy = Entropy.fromPhrase(mnemonicPhrase, languageWordList, Mnemonic24)

    entropy.value.encodeAsBase16 shouldBe expectedEntropy

    val rootKey = ExtendedPrivateKeyEd25519(entropy, "")

    (rootKey.leftKey.toArray ++
    rootKey.rightKey.toArray ++
    rootKey.chainCode.toArray).encodeAsBase16 shouldBe expectedRootSecretKey

    val rootPublicKey = rootKey.public

    (rootPublicKey.bytes.toArray ++
    rootPublicKey.chainCode.toArray).encodeAsBase16 shouldBe expectedRootPublicKey

    val `m/1852' secret` = rootKey.derive(DerivedKeyIndex.hardened(1852)).getOrThrow()

    (`m/1852' secret`.leftKey.toArray ++
    `m/1852' secret`.rightKey.toArray ++
    `m/1852' secret`.chainCode.toArray).encodeAsBase16 shouldBe `expected m/1852' secret`

    val `m/1852'/7091' secret` = `m/1852' secret`.derive(DerivedKeyIndex.hardened(7091)).getOrThrow()

    (`m/1852'/7091' secret`.leftKey.toArray ++
    `m/1852'/7091' secret`.rightKey.toArray ++
    `m/1852'/7091' secret`.chainCode.toArray).encodeAsBase16 shouldBe `expected m/1852'/7091' secret`

    val `m/1852'/7091'/0' secret` =
      `m/1852'/7091' secret`.derive(DerivedKeyIndex.hardened(0)).getOrThrow()

    (`m/1852'/7091'/0' secret`.leftKey.toArray ++
    `m/1852'/7091'/0' secret`.rightKey.toArray ++
    `m/1852'/7091'/0' secret`.chainCode.toArray).encodeAsBase16 shouldBe `expected m/1852'/7091'/0' secret`

    val `m/1852'/7091'/0'/0 secret` =
      `m/1852'/7091'/0' secret`.derive(DerivedKeyIndex.soft(0)).getOrThrow()

    (`m/1852'/7091'/0'/0 secret`.leftKey.toArray ++
    `m/1852'/7091'/0'/0 secret`.rightKey.toArray ++
    `m/1852'/7091'/0'/0 secret`.chainCode.toArray).encodeAsBase16 shouldBe `expected m/1852'/7091'/0'/0 secret`

    val `m/1852'/7091'/0'/0/0 secret` =
      `m/1852'/7091'/0'/0 secret`.derive(DerivedKeyIndex.soft(0)).getOrThrow()

    (`m/1852'/7091'/0'/0/0 secret`.leftKey.toArray ++
    `m/1852'/7091'/0'/0/0 secret`.rightKey.toArray ++
    `m/1852'/7091'/0'/0/0 secret`.chainCode.toArray).encodeAsBase16 shouldBe `expected m/1852'/7091'/0'/0/0 secret`

    val `m/1852'/7091'/0'/0/0 verify` = `m/1852'/7091'/0'/0/0 secret`.public

    (`m/1852'/7091'/0'/0/0 verify`.bytes.toArray ++
    `m/1852'/7091'/0'/0/0 verify`.chainCode.toArray).encodeAsBase16 shouldBe `expected m/1852'/7091'/0'/0/0 verify`
  }
}
