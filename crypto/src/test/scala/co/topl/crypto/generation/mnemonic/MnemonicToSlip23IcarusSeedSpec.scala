package co.topl.crypto.generation.mnemonic

import co.topl.crypto.generation.mnemonic.Language.English
import co.topl.crypto.generation.mnemonic.MnemonicSize.{Mnemonic12, Mnemonic18, Mnemonic24}
import co.topl.crypto.signing.ExtendedEd25519
import co.topl.crypto.utils.Hex.implicits._
import co.topl.models.SecretKeys.ExtendedEd25519.Length
import co.topl.models.utility.Sized
import co.topl.models.{Bytes, SecretKeys}
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

/**
 * This is an integration test of mnemonics -> entropy -> seed where the seed is a 96 byte input to the
 * ExtendedEd25519.clampBits function (the clampBits function generates a valid Bip32-Ed25519 ExtendedEd25519 secret key).
 * These tests are adopted from
 * https://github.com/input-output-hk/rust-cardano/blob/9fad3d12341acc2ab0f9c2026149af3d839447e4/cardano/src/bip/test_vectors/bip39_english.txt
 * but crucially, we are not testing the 64 byte output specified by BIP-39 (since we follow a modification of the BIP-39 spec)
 */
class MnemonicToSlip23IcarusSeedSpec
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks {

  case class SpecIn(words: String, size: MnemonicSize, language: Language, password: String)
  case class SpecOut(seed: Sized.Strict[Bytes, SecretKeys.ExtendedEd25519.Length])

  property("mnemonic should generate seed from test vector 1") {
    val specIn = SpecIn(
      "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about",
      Mnemonic12,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "9f49b8aa6610995af06dd77f4c73866fba249f398ed9fe2327726d12b4e71cad2affbb4bc18c98a6c4d7cc26f47f057a75828f4e796e78f4919591854add367f836ed9d85c9886d084efa31e300a3fdbf1cdc1ca29342355489584236a34b827".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 2") {
    val specIn = SpecIn(
      "legal winner thank year wave sausage worth useful legal winner thank yellow",
      Mnemonic12,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "d0e00d9e9c21e3d8c8e5002b175691e09546983d4472a97b49fa2fb3dfa01040daee57bf86037d648256674eaee310fcfc13e17c374d55567611c29ef7f329e880f9f7d602a730e90353c948116a088267987d08174b86b7750c3e4bb8f736e9".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 3") {
    val specIn = SpecIn(
      "letter advice cage absurd amount doctor acoustic avoid letter advice cage above",
      Mnemonic12,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "5e308762cbb38193ff709e7817668d88dc641cec0d06a475e497c482b0982d257eb50dd71a79d4cc89626b061a3a280c07d9d26d7d8dd50a657a4f258b9340fbd1b98651bbcda40c7ccac813d643c0aca4d9e76a2a7d33120686b7aedd727af2".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 4") {
    val specIn = SpecIn(
      "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo wrong",
      Mnemonic12,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "9f210be84fe8a0bb2bfa1c4c63d9c0bfeb349820220488ecdeef56f662fc011ecee823b5d525140650c8465811133afb999ea039def6ee4221c96fb7c2b1e76df7a62246c1df3a88e6b9eb0e5108392e1bcb73b4bc94982a2229533393a4e1c3".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 5") {
    val specIn = SpecIn(
      "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon agent",
      Mnemonic18,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "988d87faf022294fe3b91fbad117e1ee925d93911b8a80a0e59780ec962180c00d841ea10d5a1caab35f4f1f898338878507fd8b7d04916d91f207121fa4344d77e50f86a942af73291df41ffed148c2d2e9c65c549785aba5a728027a451939".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 6") {
    val specIn = SpecIn(
      "legal winner thank year wave sausage worth useful legal winner thank year wave sausage worth useful legal will",
      Mnemonic18,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "f9921c7492b07f3f00d78074cb9fdb87bbe4ef941bfb1c4b409268b63b332d1d8fe7ccc136556d3e73eb57369027554f4f680495ba3807e1588c96d66ebbb48c56bbbea425c5f31213ea8b217d85f6001008f6c90b6ce147b477005e1c218a07".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 7") {
    val specIn = SpecIn(
      "letter advice cage absurd amount doctor acoustic avoid letter advice cage absurd amount doctor acoustic avoid letter always",
      Mnemonic18,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "6e7e6b2ccffabb2fb3f21b814cac31a4baa6d393730e6355c56ae6b31089db301e8133a6b5622359ca8784fd0f283351297da23015a98e02f4f4ae39e7a2575866de3f1933812b7fb8be4dbdfbd6f82dc067fd6984c04dcdca3441e96ac82441".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 8") {
    val specIn = SpecIn(
      "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo when",
      Mnemonic18,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "f224b64cc3741bd515bb3e2b37ff8e6c4ae3d5c65abf3b099689460bfa0f74ffcf134de506b4ae521123ad47dbe4b9d94f626b31e726d2196677fc7045fc8d3ecc35b4bdc4ddc29348cfd01bc089869cb06979339df8bda864b67c2bb56db8c4".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 9") {
    val specIn = SpecIn(
      "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon art",
      Mnemonic24,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "d28f523cee59f6e6fc9e0ef1c7f9ecdd58378abcb1822674b1f6c45f8fd6020975e024442d00dd59c11a40837d6cc32c5cf48bf4a33ddecf5e34a96ee2791dbf28b7b2022b58f9be912d61f7af983aaee72359410039917c4c359fee565187fd".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 10") {
    val specIn = SpecIn(
      "legal winner thank year wave sausage worth useful legal winner thank year wave sausage worth useful legal winner thank year wave sausage worth title",
      Mnemonic24,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "f4056cab6caecd5475aaa0880a603a7c8c607882fcad3de68b5cf414243b2371a826d45c88ebeb62b7220c49e5c18344f5373418d9d1b6f55207d4b4c50e11d4e104a6b12feb48aadc92f83ab9c5c0b33182eaf575cc7c9cd59381e481b33fa2".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 11") {
    val specIn = SpecIn(
      "letter advice cage absurd amount doctor acoustic avoid letter advice cage absurd amount doctor acoustic avoid letter advice cage absurd amount doctor acoustic bless",
      Mnemonic24,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "cd5464452dacb5cad7d1ee20c94153a27ef7f81a17b466f26dded01071e1e5655199df1aa6ed9e263d6fc01e5cdf3cd568ebf5f36a4413bb736e8995037477bf94f9a854332ca01755fa89e90c40da3c3bee541d345f0e10e999abb6e30724cb".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 12") {
    val specIn = SpecIn(
      "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo vote",
      Mnemonic24,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "e6dfc3c0866488f40d33233c10b2401b85621998ea45936a5832debccb0abb429db0d1578a56a7841399f8a346e69d73f17e7f8168e3a0b9c6859a242270003785cbbd776846b7da0a634b99941c8caa6381a980bad2a1f15cfd14976fc3a224".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 13") {
    val specIn = SpecIn(
      "ozone drill grab fiber curtain grace pudding thank cruise elder eight picnic",
      Mnemonic12,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "f745ca9296e5a9107698da435ecaa6c6f910b80414d7efde1402950c855093c08b2fed74d39ee250b7cf781f239908833e6cb594814a174b38c6a3bedc8c626f5886298ee16313b26f39c5dd5138f5c4f5915edf4442193ffc356d9590d67bcd".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 14") {
    val specIn = SpecIn(
      "gravity machine north sort system female filter attitude volume fold club stay feature office ecology stable narrow fog",
      Mnemonic18,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "d2154b7f6be0b0ea8aba073e1aae7404af5fa552b4de2d49e8bf578b232e8234aa613691ff6aab299ff6d4c74c074af637ed5d6096d1430f7ad50f0ea95036a6f6e1c288597ca47c37ca128383fb1df241d4cdab204bc2fcaf39a46a6586accc".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 15") {
    val specIn = SpecIn(
      "hamster diagram private dutch cause delay private meat slide toddler razor book happy fancy gospel tennis maple dilemma loan word shrug inflict delay length",
      Mnemonic24,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "0476e768d47f5f1c171576ac5c08bd4f96bdfd03ff76ab10469720f46d78aab22c33780ca0c943c3b6997b10faf1e1c89293eb5d8b783e762fa3e4f94c64637a0e1e4435262be179a99f80c9852fac58465ee4c4a9df8ee7c2db957b45447745".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 16") {
    val specIn = SpecIn(
      "scheme spot photo card baby mountain device kick cradle pact join borrow",
      Mnemonic12,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "2adc2406c5505b268aa9880272c4537504f9d76b11e4b6bbc8d5af8952a0fab3ee5f330723d86f37a2ea2356a112806debbc4771ebe75ac2fa0ec269417c756930c2a7a015ae34fd06b7fc4f43aa4ab778cd2f952a3df7d13a168a922d527acd".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 17") {
    val specIn = SpecIn(
      "horn tenant knee talent sponsor spell gate clip pulse soap slush warm silver nephew swap uncle crack brave",
      Mnemonic18,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "d8fdac20cd7486c050a1c60dd95edca223ec3268a1e333463dc993cd437e0421620b1631a13de32cc2977d818f21306b739465589b2120e484577d88a4ddf107c043022fc657912dafe0a8ca0e7827cf0aaf4379acefe12b5e38696f80515b50".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 18") {
    val specIn = SpecIn(
      "panda eyebrow bullet gorilla call smoke muffin taste mesh discover soft ostrich alcohol speed nation flash devote level hobby quick inner drive ghost inside",
      Mnemonic24,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "bc44258f5a73438d866145ea21844a287ae946680b237068f6dbac5b9174a695e2c40d224400deab9570548755ebfebf15ab1818451d9619a87523cc1e80c0196954bf51ebe212682dfe1c667afa33d551e6fc35667858e3771445c5ad96567a".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 19") {
    val specIn = SpecIn(
      "cat swing flag economy stadium alone churn speed unique patch report train",
      Mnemonic12,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "c52dc95377adb2d0d7cd3b10c1ed81e6bcec9f59c1c67186777fd1188939ebd6e8551359ff9affb65e32b997ec560fcbca97e3416bac245411fc6069aaa3dc855bdfaef411f1f0b3195353d94900fda743c8b5005cf17110eca07825ddea901a".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 20") {
    val specIn = SpecIn(
      "light rule cinnamon wrap drastic word pride squirrel upgrade then income fatal apart sustain crack supply proud access",
      Mnemonic18,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "93b68014813d5132bf9fcc342096922cb7e421eca414f8bf7e8a3574513115d69bd184b2c1c817a6095385596f9c46583cbd69903e7241aa561efee18b64e796ddf26f5cf7ccb4f9831caae70d278c02e0da4fdee278b86cd44a747fcba43b2a".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 21") {
    val specIn = SpecIn(
      "all hour make first leader extend hole alien behind guard gospel lava path output census museum junior mass reopen famous sing advance salt reform",
      Mnemonic24,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "1392aabe335aa2472394d74ea034e17e8af740c26983e92556c1b0614a05f4fe20bd70746db174addae3fdb9e6f1feebff068c81214c4132b7e291c0cd3f6c0635e025373627e862c7631d96a92a8a76ce9e5c74d40165ce604eb486a3da70ed".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 22") {
    val specIn = SpecIn(
      "vessel ladder alter error federal sibling chat ability sun glass valve picture",
      Mnemonic12,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "6f2629e1389815f097f40a43a0654c66344dd42d2c6b6666ebb202968ebcefd450b0ee9aff4fbe065ad143d934a7ba63b2149c21f74b589345cfaa2d8b36ebc8f39e768c341c41e12126c851db939164647cc80c15e25a358e329c2aa7a77d81".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 23") {
    val specIn = SpecIn(
      "scissors invite lock maple supreme raw rapid void congress muscle digital elegant little brisk hair mango congress clump",
      Mnemonic18,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "e4869cc1aaba2a68f6ac990445e82077f8779818c50fd2b256df016348d7f3623d9ae35187314cbe6e59a0b293a8f0b46559e8d9308f0559b839110f28ca20b6dad7c652bf9fc90003a41d1324e495811976efe95d9f8d28993d0e7a94187cdf".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

  property("mnemonic should generate seed from test vector 24") {
    val specIn = SpecIn(
      "void come effort suffer camp survey warrior heavy shoot primary clutch crush open amazing screen patrol group space point ten exist slush involve unfold",
      Mnemonic24,
      English,
      "TREZOR"
    )
    val specOut = SpecOut(
      "7623751ff9c9ecd5f62c4abc17ea3166664d351b12f48d83a1c6b5e3374efc3aae7651566816270d046a1de788b297f8ec9ab822d9b3bc3494fe8205197976082af0974201b41e97e7d7c304297ebecbba1308463e81aa69d170de87f0ab15f9".unsafeStrictBytes
    )

    val seed: Sized.Strict[Bytes, Length] =
      FromEntropy.derive(specIn.words, specIn.size, specIn.language)(e => e) match {
        case Left(_)      => throw new Error("error deriving entropy from words")
        case Right(value) => ExtendedEd25519.entropyToSeed(value)(specIn.password)
      }

    seed shouldBe specOut.seed
  }

}
