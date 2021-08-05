package co.topl.attestation.keyManagement.mnemonic

import co.topl.attestation.keyManagement.mnemonic.Mnemonic._
import co.topl.utils.CommonGenerators
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.StringDataTypes.Base16Data
import org.scalacheck.Gen
import org.scalatest.matchers.must.Matchers.{be, not}
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import co.topl.utils.SizedByteCollection.implicits._
import co.topl.utils.codecs.implicits._
import co.topl.attestation.keyManagement.derivedKeys.ExtendedPrivateKeyEd25519.implicits._

class MnemonicSpec
    extends AnyPropSpec
    with CommonGenerators
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks {

  property("12 phrase mnemonic with valid words should be valid") {
    val phrase = "cat swing flag economy stadium alone churn speed unique patch report train"
    val mnemonic = Mnemonic.derive(phrase, Mnemonic12, English)

    mnemonic.isRight shouldBe true
  }

  property("12 phrase mnemonic with invalid word length should be invalid") {
    val phrase = "result fresh margin life life filter vapor trim"

    val mnemonic = Mnemonic.derive(phrase, Mnemonic12, English)

    mnemonic shouldBe Left(InvalidWordLength())
  }

  property("12 phrase mnemonic with invalid words should be invalid") {
    val phrase = "amber glue hallway can truth drawer wave flex cousin grace close compose"

    val mnemonic = Mnemonic.derive(phrase, Mnemonic12, English)

    mnemonic shouldBe Left(InvalidWords())
  }

  property("12 phrase mnemonic with valid words and invalid checksum should be invalid") {
    val phrase = "ugly wire busy skate slice kidney razor eager bicycle struggle aerobic picnic"
    val mnemonic = Mnemonic.derive(phrase, Mnemonic12, English)

    mnemonic shouldBe Left(InvalidChecksum())
  }

  property("phrase should output the same private key if same password") {
    forAll(stringGen) { password =>
      val phrase = "ozone drill grab fiber curtain grace pudding thank cruise elder eight picnic"
      val mnemonic = Mnemonic.derive(phrase, Mnemonic12, English).getOrElse(throw new Error("Invalid mnemonic!"))

      val firstAttempt = mnemonic(password)
      val secondAttempt = mnemonic(password)

      firstAttempt.leftKey shouldBe secondAttempt.leftKey
      firstAttempt.rightKey shouldBe secondAttempt.rightKey
      firstAttempt.chainCode shouldBe secondAttempt.chainCode
    }
  }

  property("phrase should output a different private key if different password") {
    forAll(stringGen, stringGen) { (password1, password2) =>
      val phrase = "ozone drill grab fiber curtain grace pudding thank cruise elder eight picnic"
      val keyGen = Mnemonic.derive(phrase, Mnemonic12, English).getOrElse(throw new Error("Invalid mnemonic!"))

      val firstAttempt = keyGen(password1)
      val secondAttempt = keyGen(password2)

      if (password1 != password2) {
        firstAttempt.leftKey should not be secondAttempt.leftKey
        firstAttempt.rightKey should not be secondAttempt.rightKey
        firstAttempt.chainCode should not be secondAttempt.chainCode
      }
    }
  }

  property("from UUID should return valid mnemonic with size 12") {
    forAll(Gen.uuid) { uuid =>
      val mnemonic = Mnemonic.derive(uuid, English)

      mnemonic.isRight shouldBe true
    }
  }

  def entropyLengthTest(bytes: Int, expected: MnemonicSize): Unit =
    property(s"from entropy of length $bytes should be valid") {
      forAll(specificLengthBytesGen(bytes)) { entropy =>
        if (entropy.length == bytes) {
          val mnemonic = Mnemonic.derive(entropy, expected, English)

          mnemonic.isRight shouldBe true
        }
      }
    }

  entropyLengthTest(16, Mnemonic12)
  entropyLengthTest(20, Mnemonic15)
  entropyLengthTest(24, Mnemonic18)
  entropyLengthTest(28, Mnemonic21)
  entropyLengthTest(32, Mnemonic24)

  property("mnemonic with extra whitespace is valid") {
    val mnemonic = Mnemonic.derive(
      "vessel ladder alter error  federal sibling chat   ability sun glass valve picture",
      Mnemonic12,
      English
    )

    mnemonic.isRight shouldBe true
  }

  property("mnemonic with extra whitespace has same seed as single spaced") {
    val password = ""

    val expectedKey = Mnemonic
      .derive(
        "vessel ladder alter error federal sibling chat ability sun glass valve picture",
        Mnemonic12,
        English
      )
      .getOrThrow()(password)

    val result = Mnemonic
      .derive(
        "vessel ladder alter error  federal sibling chat   ability sun glass valve picture",
        Mnemonic12,
        English
      )
      .getOrThrow()(password)

    result.leftKey shouldBe expectedKey.leftKey
    result.rightKey shouldBe expectedKey.rightKey
    result.chainCode shouldBe expectedKey.chainCode
  }

  property("mnemonic with capital letters is valid") {
    val mnemonic = Mnemonic.derive(
      "Legal Winner Thank Year Wave Sausage Worth Useful Legal " +
      "Winner Thank Year Wave Sausage Worth Useful Legal Will",
      Mnemonic18,
      English
    )

    mnemonic.isRight shouldBe true
  }

  property("mnemonic with capital letters has same seed as lowercase") {
    val password = ""

    val expectedKey = Mnemonic
      .derive(
        "legal winner thank year wave sausage worth useful legal " +
        "winner thank year wave sausage worth useful legal will",
        Mnemonic18,
        English
      )
      .getOrThrow()(password)

    val result = Mnemonic
      .derive(
        "Legal Winner Thank Year Wave Sausage Worth Useful Legal " +
        "Winner Thank Year Wave Sausage Worth Useful Legal Will",
        Mnemonic18,
        English
      )
      .getOrThrow()(password)

    result.leftKey shouldBe expectedKey.leftKey
    result.rightKey shouldBe expectedKey.rightKey
    result.chainCode shouldBe expectedKey.chainCode
  }

  property("mnemonic with unusual characters is invalid") {
    val mnemonic = Mnemonic.derive(
      "voi\uD83D\uDD25d come effort suffer camp su\uD83D\uDD25rvey warrior heavy shoot primary" +
      " clutch c\uD83D\uDD25rush" +
      " open amazing screen " +
      "patrol group space point ten exist slush inv\uD83D\uDD25olve unfold",
      Mnemonic24,
      English
    )

    mnemonic shouldBe Left(InvalidWords())
  }

  case class TestVector(
    name:       String,
    phrase:     String,
    size:       MnemonicSize,
    language:   Language,
    password:   String,
    privateKey: String
  )

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

      val pkResult = Mnemonic.derive(tv.phrase, tv.size, tv.language).getOrThrow()(tv.password)

      val pkResultBase16 =
        (pkResult.leftKey.toVector ++ pkResult.rightKey.toVector ++ pkResult.chainCode.toVector).encodeAsBase16

      pkResultBase16 shouldBe expectedPrivateKeyBase16
    }

  testVectors.foreach(testVectorTest)
}
