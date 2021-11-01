package co.topl.crypto.mnemonic

import co.topl.crypto.mnemonic.FromEntropy.Instances._
import co.topl.crypto.mnemonic.FromEntropy.derive
import co.topl.crypto.mnemonic.Language._
import co.topl.crypto.mnemonic.MnemonicSize._
import co.topl.crypto.utils.Generators._
import co.topl.crypto.utils.Hex.implicits._
import co.topl.models.SecretKeys
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class MnemonicToExtendedEd25519
    extends AnyFlatSpec
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  case class TestVector(name: String, specIn: SpecIn, specOut: SpecOut)
  case class SpecIn(phrase: String, size: MnemonicSize, language: Language, password: String)
  case class SpecOut(sk: SecretKeys.ExtendedEd25519)

  "Key from mnemonic phrase" should "output same the key with the same password" in {
    val createKey: String => SecretKeys.ExtendedEd25519 =
      derive[String => SecretKeys.ExtendedEd25519](
        "ozone drill grab fiber curtain grace pudding thank cruise elder eight picnic",
        Mnemonic12,
        English
      ) match {
        case Left(value)  => throw new Error(s"Problem deriving SecretKeys.ExtendedEd25519 from mnemonic: $value")
        case Right(value) => value
      }

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
      ) match {
        case Left(value)  => throw new Error(s"Problem deriving SecretKeys.ExtendedEd25519 from mnemonic: $value")
        case Right(value) => value
      }

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
      SpecOut(
        SecretKeys.ExtendedEd25519(
          "49275d5103766daaf068000326cfa0b8b18389967c1995eee2cc36fc66d2d610".unsafeStrictBytes,
          "7e83aaa8f1afc49ffb6b5f7755b813134c98d60523d2b2f0eeea5145d50c03ff".unsafeStrictBytes,
          "36bf96ba09651007f338c4dfc4355f79fa3f947a68037e8976b2d8eac63d7743".unsafeStrictBytes
        )
      )
    )

    val sk = FromEntropy
      .derive[String => SecretKeys.ExtendedEd25519](tv.specIn.phrase, tv.specIn.size, tv.specIn.language) match {
      case Left(value)  => throw new Error(s"failed test: $value")
      case Right(value) => value(tv.specIn.password)
    }

    sk shouldBe tv.specOut.sk
  }
  it should "satisfy test vector 2" in {
    val tv = TestVector(
      "Test Vector #2",
      SpecIn(
        "vessel erase town arrow girl emotion siren better fork approve spare convince sauce amused clap",
        Mnemonic15,
        English,
        "heart",
      ),
      SpecOut(
        SecretKeys.ExtendedEd25519(
          "4702e7f65dbd2af89c3959cdb4e79f72552102a61d89ad9918eb89a259065c10".unsafeStrictBytes,
          "9dc74fe3f9a498114f2656a8be4f93831dbc5d94b43fd4416602deefd8cfa414".unsafeStrictBytes,
          "ed2f79a789e55ee34e8c186c56af710017b85a9497418f62ada9b26c2a98d5ad".unsafeStrictBytes
        )
      )
    )

    val sk = FromEntropy
      .derive[String => SecretKeys.ExtendedEd25519](tv.specIn.phrase, tv.specIn.size, tv.specIn.language) match {
      case Left(value)  => throw new Error(s"failed test: $value")
      case Right(value) => value(tv.specIn.password)
    }

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
        "describe",
      ),
      SpecOut(
        SecretKeys.ExtendedEd25519(
          "5ec220a0c63beccc8b273a60dd7968ed9b10c6667e435bab39514479718c01f8".unsafeStrictBytes,
          "6a61b32fcdc5d799b327a62d968a7331f773a6ad64b1684b0a1f35b6149a0ad1".unsafeStrictBytes,
          "4ecf48d6c5e2d7918c5405ebba037a931ae29dc7de59815a7c3af6aa5b4dd367".unsafeStrictBytes
        )
      )
    )

    val sk = FromEntropy
      .derive[String => SecretKeys.ExtendedEd25519](tv.specIn.phrase, tv.specIn.size, tv.specIn.language) match {
      case Left(value)  => throw new Error(s"failed test: $value")
      case Right(value) => value(tv.specIn.password)

    }

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
      SpecOut(
        SecretKeys.ExtendedEd25519(
          "5555589c9bf2dfcfcf70c2199a6c59631ab04cd5898c3a2cb3e63ece6f71e790".unsafeStrictBytes,
          "7a3c9e2d94881a051f75d729b41b5ffc15224b457c2611348a5513413d8dbabe".unsafeStrictBytes,
          "052ce0b1aaed6d6429a3a5dcb3b364e0876d63ab2f3a87d36bf5065b3b50618c".unsafeStrictBytes
        )
      )
    )

    val sk = FromEntropy
      .derive[String => SecretKeys.ExtendedEd25519](tv.specIn.phrase, tv.specIn.size, tv.specIn.language) match {
      case Left(value)  => throw new Error(s"failed test: $value")
      case Right(value) => value(tv.specIn.password)

    }

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
      SpecOut(
        SecretKeys.ExtendedEd25519(
          "5f65b9988a25ed64c1ab99a31dfb87864067bdb236262a6d3a735ae2072edf98".unsafeStrictBytes,
          "9ffb42972d707a282b77fb270e7c9afea86e868a162f0c7ad7e225a17d509719".unsafeStrictBytes,
          "6124807717e882b56ea96c8acb15d45b867a48b6c91ee05ddf96651393084be1".unsafeStrictBytes
        )
      )
    )

    val sk = FromEntropy
      .derive[String => SecretKeys.ExtendedEd25519](tv.specIn.phrase, tv.specIn.size, tv.specIn.language) match {
      case Left(value)  => throw new Error(s"failed test: $value")
      case Right(value) => value(tv.specIn.password)

    }

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
      SpecOut(
        SecretKeys.ExtendedEd25519(
          "5385aad34e831743a6acd89017628a86bcbf93d693abc34bc1d2d393f8576668".unsafeStrictBytes,
          "5716f1bff83f4e5b7380246d4ab859041c62abae17863bbb52b82c434a098d7d".unsafeStrictBytes,
          "5bc52f2580c9c7728426d8edd5e58c72412212e41d87b5af3e63b7c8d3aa4a94".unsafeStrictBytes
        )
      )
    )

    val sk = FromEntropy
      .derive[String => SecretKeys.ExtendedEd25519](tv.specIn.phrase, tv.specIn.size, tv.specIn.language) match {
      case Left(value)  => throw new Error(s"failed test: $value")
      case Right(value) => value(tv.specIn.password)

    }

    sk shouldBe tv.specOut.sk
  }
}
