package co.topl.crypto.generation

import co.topl.crypto.generation.mnemonic.Language.English
import co.topl.crypto.generation.mnemonic.MnemonicSizes._
import co.topl.crypto.generation.mnemonic.{Entropy, Language, MnemonicSize, ToEntropy}
import co.topl.crypto.utils.Generators.genByteArrayOfSize
import co.topl.crypto.utils.Hex
import co.topl.models.utility.Base58
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class MnemonicSpec extends AnyPropSpec with ScalaCheckPropertyChecks with ScalaCheckDrivenPropertyChecks {

  implicit val entropyAsString: ToEntropy[String] =
    (e: Entropy) => Base58.encode(e.value)

  property("12 phrase mnemonic with valid words should be valid") {
    val phrase = "cat swing flag economy stadium alone churn speed unique patch report train"
    val mnemonic = ToEntropy.derive[String](phrase, `12`, English)

    mnemonic.isRight shouldBe true
  }

  property("12 phrase mnemonic with invalid word length should be invalid") {
    val phrase = "result fresh margin life life filter vapor trim"

    val mnemonic = ToEntropy.derive[String](phrase, `12`, English)

    mnemonic.isLeft shouldBe true
  }

  property("12 phrase mnemonic with invalid words should be invalid") {
    val phrase = "amber glue hallway can truth drawer wave flex cousin grace close compose"

    val mnemonic = ToEntropy.derive[String](phrase, `12`, English)

    mnemonic.isLeft shouldBe true
  }

  property("12 phrase mnemonic with valid words and invalid checksum should be invalid") {
    val phrase = "ugly wire busy skate slice kidney razor eager bicycle struggle aerobic picnic"
    val mnemonic = ToEntropy.derive[String](phrase, `12`, English)

    mnemonic.isLeft shouldBe true
  }

  def entropyLengthTest(bytes: Int, size: MnemonicSize): Unit =
    property(s"from entropy of length $bytes should be valid") {
      forAll(genByteArrayOfSize(bytes)) { entropy: Array[Byte] =>
        if (entropy.length == bytes) {
          val entropyString = ToEntropy.derive[String](entropy, size)

          entropyString.isRight shouldBe true
        }
      }
    }

  entropyLengthTest(16, `12`)
  entropyLengthTest(20, `15`)
  entropyLengthTest(24, `18`)
  entropyLengthTest(28, `21`)
  entropyLengthTest(32, `24`)

  property("mnemonic with extra whitespace is valid") {
    val mnemonic =
      ToEntropy.derive[String](
        "vessel ladder alter error  federal sibling chat   ability sun glass valve picture",
        `12`,
        English
      )

    mnemonic.isRight shouldBe true
  }

  property("mnemonic with extra whitespace has same value as single spaced") {
    val expected =
      ToEntropy.derive[String](
        "vessel ladder alter error federal sibling chat ability sun glass valve picture",
        `12`,
        English
      ) match {
        case Left(value)  => throw new Error("failed test")
        case Right(value) => value
      }

    val result =
      ToEntropy.derive[String](
        "vessel ladder alter error  federal sibling chat   ability sun glass valve picture",
        `12`,
        English
      ) match {
        case Left(value)  => throw new Error("failed test")
        case Right(value) => value
      }

    result shouldBe expected
  }

  property("mnemonic with capital letters is valid") {
    val mnemonic = ToEntropy.derive[String](
      "Legal Winner Thank Year Wave Sausage Worth Useful Legal " +
      "Winner Thank Year Wave Sausage Worth Useful Legal Will",
      `18`,
      English
    )

    mnemonic.isRight shouldBe true
  }

  property("mnemonic with capital letters has same entropy as lowercase") {
    val expectedEntropy =
      ToEntropy.derive[String](
        "legal winner thank year wave sausage worth useful legal " +
        "winner thank year wave sausage worth useful legal will",
        `18`,
        English
      ) match {
        case Left(value)  => throw new Error("failed test")
        case Right(value) => value
      }

    val result =
      ToEntropy.derive[String](
        "Legal Winner Thank Year Wave Sausage Worth Useful Legal " +
        "Winner Thank Year Wave Sausage Worth Useful Legal Will",
        `18`,
        English
      ) match {
        case Left(value)  => throw new Error("failed test")
        case Right(value) => value
      }

    result shouldBe expectedEntropy
  }

  property("mnemonic with unusual characters is invalid") {
    val entropy =
      ToEntropy.derive[String](
        "voi\uD83D\uDD25d come effort suffer camp su\uD83D\uDD25rvey warrior heavy shoot primary" +
        " clutch c\uD83D\uDD25rush" +
        " open amazing screen " +
        "patrol group space point ten exist slush inv\uD83D\uDD25olve unfold",
        `24`,
        English
      )

    entropy.isLeft shouldBe true
  }

  case class SpecIn(words: String, size: MnemonicSize, language: Language)
  case class SpecOut(entropy: Entropy)

  property("mnemonic should generating entropy from test vector 1") {
    val specIn = SpecIn(
      "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about",
      `12`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("00000000000000000000000000000000")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 2") {
    val specIn = SpecIn(
      "legal winner thank year wave sausage worth useful legal winner thank yellow",
      `12`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 3") {
    val specIn = SpecIn(
      "letter advice cage absurd amount doctor acoustic avoid letter advice cage above",
      `12`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("80808080808080808080808080808080")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 4") {
    val specIn = SpecIn(
      "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo wrong",
      `12`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("ffffffffffffffffffffffffffffffff")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 5") {
    val specIn = SpecIn(
      "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon agent",
      `18`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("000000000000000000000000000000000000000000000000")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 6") {
    val specIn = SpecIn(
      "legal winner thank year wave sausage worth useful legal winner thank year wave sausage worth useful legal will",
      `18`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 7") {
    val specIn = SpecIn(
      "letter advice cage absurd amount doctor acoustic avoid letter advice cage absurd amount doctor acoustic avoid letter always",
      `18`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("808080808080808080808080808080808080808080808080")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 8") {
    val specIn = SpecIn(
      "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo when",
      `18`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("ffffffffffffffffffffffffffffffffffffffffffffffff")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 9") {
    val specIn = SpecIn(
      "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon art",
      `24`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("0000000000000000000000000000000000000000000000000000000000000000")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 10") {
    val specIn = SpecIn(
      "legal winner thank year wave sausage worth useful legal winner thank year wave sausage worth useful legal winner thank year wave sausage worth title",
      `24`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 11") {
    val specIn = SpecIn(
      "letter advice cage absurd amount doctor acoustic avoid letter advice cage absurd amount doctor acoustic avoid letter advice cage absurd amount doctor acoustic bless",
      `24`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("8080808080808080808080808080808080808080808080808080808080808080")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 12") {
    val specIn = SpecIn(
      "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo vote",
      `24`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 13") {
    val specIn = SpecIn(
      "ozone drill grab fiber curtain grace pudding thank cruise elder eight picnic",
      `12`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("9e885d952ad362caeb4efe34a8e91bd2")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 14") {
    val specIn = SpecIn(
      "gravity machine north sort system female filter attitude volume fold club stay feature office ecology stable narrow fog",
      `18`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("6610b25967cdcca9d59875f5cb50b0ea75433311869e930b")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 15") {
    val specIn = SpecIn(
      "hamster diagram private dutch cause delay private meat slide toddler razor book happy fancy gospel tennis maple dilemma loan word shrug inflict delay length",
      `24`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("68a79eaca2324873eacc50cb9c6eca8cc68ea5d936f98787c60c7ebc74e6ce7c")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 16") {
    val specIn = SpecIn(
      "scheme spot photo card baby mountain device kick cradle pact join borrow",
      `12`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("c0ba5a8e914111210f2bd131f3d5e08d")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 17") {
    val specIn = SpecIn(
      "horn tenant knee talent sponsor spell gate clip pulse soap slush warm silver nephew swap uncle crack brave",
      `18`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("6d9be1ee6ebd27a258115aad99b7317b9c8d28b6d76431c3")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 18") {
    val specIn = SpecIn(
      "panda eyebrow bullet gorilla call smoke muffin taste mesh discover soft ostrich alcohol speed nation flash devote level hobby quick inner drive ghost inside",
      `24`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("9f6a2878b2520799a44ef18bc7df394e7061a224d2c33cd015b157d746869863")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 19") {
    val specIn = SpecIn(
      "cat swing flag economy stadium alone churn speed unique patch report train",
      `12`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("23db8160a31d3e0dca3688ed941adbf3")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 20") {
    val specIn = SpecIn(
      "light rule cinnamon wrap drastic word pride squirrel upgrade then income fatal apart sustain crack supply proud access",
      `18`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("8197a4a47f0425faeaa69deebc05ca29c0a5b5cc76ceacc0")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 21") {
    val specIn = SpecIn(
      "all hour make first leader extend hole alien behind guard gospel lava path output census museum junior mass reopen famous sing advance salt reform",
      `24`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("066dca1a2bb7e8a1db2832148ce9933eea0f3ac9548d793112d9a95c9407efad")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 22") {
    val specIn = SpecIn(
      "vessel ladder alter error federal sibling chat ability sun glass valve picture",
      `12`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("f30f8c1da665478f49b001d94c5fc452")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 23") {
    val specIn = SpecIn(
      "scissors invite lock maple supreme raw rapid void congress muscle digital elegant little brisk hair mango congress clump",
      `18`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("c10ec20dc3cd9f652c7fac2f1230f7a3c828389a14392f05")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }

  property("mnemonic should generating entropy from test vector 24") {
    val specIn = SpecIn(
      "void come effort suffer camp survey warrior heavy shoot primary clutch crush open amazing screen patrol group space point ten exist slush involve unfold",
      `24`,
      English
    )
    val specOut = SpecOut(Entropy(Hex.decode("f585c11aec520db57dd353c69554b21a89b20fb0650966fa0a9d6f74fd989d8f")))

    val entropy: Entropy = ToEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
      case Left(_)      => throw new Error("error deriving entropy from words")
      case Right(value) => value
    }

    entropy shouldBe specOut.entropy
  }
}
