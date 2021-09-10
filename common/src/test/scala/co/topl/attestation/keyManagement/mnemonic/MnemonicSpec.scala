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

    val expectedRootSecretKey = Array(96, 249, 52, 37, 2, 242, 129, 98, 144, 42, 35, 10, 200, 70, 234, 226, 191, 128,
      189, 162, 101, 199, 33, 204, 244, 162, 251, 123, 224, 5, 169, 82, 186, 45, 243, 37, 227, 205, 182, 7, 208, 57,
      128, 99, 76, 154, 225, 153, 74, 251, 187, 85, 255, 203, 211, 15, 37, 159, 28, 208, 47, 147, 223, 181, 210, 29, 46,
      86, 214, 104, 176, 227, 183, 2, 178, 90, 191, 101, 200, 238, 47, 13, 252, 193, 55, 195, 171, 182, 252, 40, 230, 7,
      224, 166, 150, 38).map(_ - 128).map(_.toByte).encodeAsBase16

    val expectedRootPublicKey = Array(253, 64, 158, 130, 189, 225, 97, 110, 184, 208, 233, 128, 2, 39, 175, 217, 100,
      136, 124, 89, 136, 15, 61, 206, 117, 233, 129, 25, 77, 162, 54, 127, 210, 29, 46, 86, 214, 104, 176, 227, 183, 2,
      178, 90, 191, 101, 200, 238, 47, 13, 252, 193, 55, 195, 171, 182, 252, 40, 230, 7, 224, 166, 150, 38)
      .map(_ - 128)
      .map(_.toByte)
      .encodeAsBase16

    val `expected m/1852' secret` = Array(152, 193, 146, 176, 14, 80, 232, 241, 116, 247, 86, 132, 223, 27, 66, 200,
      150, 212, 89, 116, 18, 243, 128, 224, 96, 49, 229, 51, 229, 5, 169, 82, 106, 43, 229, 118, 94, 193, 114, 12, 86,
      189, 167, 107, 189, 168, 237, 126, 169, 20, 136, 97, 76, 57, 51, 66, 178, 121, 128, 194, 75, 168, 42, 214, 98,
      237, 219, 103, 97, 162, 146, 58, 67, 30, 220, 241, 45, 149, 80, 27, 29, 197, 206, 1, 42, 112, 205, 187, 169, 247,
      184, 155, 199, 107, 66, 45).map(_ - 128).map(_.toByte).encodeAsBase16

    val `expected m/1852'/7091' secret` = Array(40, 90, 59, 48, 248, 49, 113, 29, 19, 184, 66, 109, 175, 169, 70, 220,
      45, 64, 138, 174, 128, 229, 15, 51, 229, 239, 238, 55, 235, 5, 169, 82, 228, 168, 119, 19, 201, 154, 10, 225, 200,
      54, 244, 178, 24, 230, 20, 67, 166, 214, 37, 84, 138, 53, 255, 152, 80, 1, 160, 176, 98, 18, 142, 192, 122, 108,
      49, 21, 118, 56, 155, 14, 163, 226, 5, 37, 57, 242, 128, 132, 89, 211, 248, 233, 111, 28, 165, 79, 18, 253, 12,
      241, 164, 222, 15, 234).map(_ - 128).map(_.toByte).encodeAsBase16

    val `expected m/1852'/7091'/0' secret` = Array(176, 59, 153, 97, 81, 27, 213, 102, 130, 90, 7, 172, 220, 215, 59,
      140, 197, 161, 150, 216, 53, 93, 95, 80, 147, 225, 67, 135, 237, 5, 169, 82, 215, 133, 92, 157, 179, 210, 75, 155,
      79, 22, 46, 166, 63, 198, 171, 234, 229, 112, 8, 21, 139, 162, 126, 189, 149, 23, 87, 77, 125, 88, 45, 230, 47,
      185, 9, 7, 186, 1, 143, 212, 192, 81, 74, 152, 63, 237, 92, 120, 89, 96, 94, 26, 15, 204, 242, 14, 109, 236, 185,
      129, 214, 240, 4, 71).map(_ - 128).map(_.toByte).encodeAsBase16

    val `expected m/1852'/7091'/0'/0 secret` = Array(112, 68, 174, 39, 151, 26, 100, 186, 188, 113, 177, 244, 77, 197,
      201, 100, 104, 35, 67, 78, 203, 229, 184, 82, 15, 59, 131, 63, 245, 5, 169, 82, 171, 97, 70, 255, 76, 38, 242,
      159, 109, 129, 5, 53, 37, 92, 57, 143, 163, 157, 198, 183, 210, 217, 250, 11, 105, 114, 146, 43, 164, 153, 205,
      221, 185, 33, 31, 242, 112, 76, 96, 202, 208, 43, 39, 246, 139, 211, 223, 194, 237, 91, 23, 14, 108, 196, 158,
      221, 177, 106, 68, 20, 30, 135, 198, 150).map(_ - 128).map(_.toByte).encodeAsBase16

    val `expected m/1852'/7091'/0'/0/0 secret` = Array(0, 132, 28, 86, 117, 12, 232, 42, 163, 16, 242, 103, 47, 78, 138,
      169, 113, 34, 49, 107, 185, 180, 13, 151, 117, 168, 141, 84, 250, 5, 169, 82, 115, 227, 62, 246, 128, 140, 73,
      212, 183, 103, 104, 125, 7, 142, 119, 177, 77, 227, 198, 198, 46, 74, 34, 98, 130, 129, 24, 18, 143, 114, 128,
      152, 19, 36, 64, 40, 223, 42, 120, 38, 23, 203, 38, 210, 248, 44, 87, 219, 223, 73, 72, 251, 140, 90, 255, 147,
      70, 160, 168, 255, 10, 0, 149, 165).map(_ - 128).map(_.toByte).encodeAsBase16

    val `expected m/1852'/7091'/0'/0/0 verify` = Array(226, 35, 14, 109, 119, 178, 28, 0, 125, 142, 242, 28, 228, 8, 46,
      88, 201, 48, 146, 95, 139, 126, 130, 228, 108, 222, 47, 149, 215, 168, 44, 84, 19, 36, 64, 40, 223, 42, 120, 38,
      23, 203, 38, 210, 248, 44, 87, 219, 223, 73, 72, 251, 140, 90, 255, 147, 70, 160, 168, 255, 10, 0, 149, 165)
      .map(_ - 128)
      .map(_.toByte)
      .encodeAsBase16

    val languageWordList = LanguageWordList.validated(English).getOrThrow()

    val mnemonicPhrase = Phrase.validated(mnemonic, Mnemonic24, languageWordList).getOrThrow()

    val entropy = Entropy.fromPhrase(mnemonicPhrase, languageWordList, Mnemonic24)

    val rootTest = ExtendedPrivateKeyEd25519(Entropy.validated(expectedEntropy.value, Mnemonic24).getOrThrow(), "")

    import cats.implicits._

    println((rootTest.leftKey.toArray ++ rootTest.rightKey.toArray ++ rootTest.chainCode.toArray).encodeAsBase16.show)

//    entropy.value.encodeAsBase16 shouldBe expectedEntropy

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
