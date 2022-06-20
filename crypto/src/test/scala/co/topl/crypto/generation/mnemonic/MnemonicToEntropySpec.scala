//package co.topl.crypto.generation
//
//import cats.scalatest.EitherValues
//import co.topl.crypto.generation.mnemonic.Language.{English, LanguageWordList}
//import co.topl.crypto.generation.mnemonic.MnemonicSizes._
//import co.topl.crypto.generation.mnemonic.{Entropy, MnemonicSize, MnemonicSizes, Phrase}
//import co.topl.crypto.signing.Curve25519
//import co.topl.crypto.utils.Hex.implicits.Ops
//import co.topl.crypto.utils.TestVector
//import co.topl.models.SecretKeys
//import co.topl.models.utility.Lengths
//import io.circe.Decoder
//import io.circe.generic.semiauto.deriveDecoder
//import org.scalatest.matchers.should.Matchers
//import org.scalatest.propspec.AnyPropSpec
//import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
//
//class MnemonicToEntropySpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with EitherValues {
//
//  case class SpecInputs(mnemonic: Phrase, mnemonicSize: MnemonicSize, passphrase: String)
//  case class SpecOutputs(entropy: Entropy)
//  case class MnemonicToSeedTestVector(inputs: SpecInputs, outputs: SpecOutputs) extends TestVector
//
//  implicit val inputsDecoder: Decoder[SpecInputs] = deriveDecoder[SpecInputs]
//  implicit val outputsDecoder: Decoder[SpecOutputs] = deriveDecoder[SpecOutputs]
//  implicit val testVectorDecoder: Decoder[MnemonicToSeedTestVector] = deriveDecoder[MnemonicToSeedTestVector]
//
//  val testVectors: List[MnemonicToSeedTestVector] = TestVector.read("MnemonicToSeedTestVectors.json")
//
//  private val wordList = LanguageWordList.validated(English) match {
//    case Left(err)   => throw new Exception(s"Could not load English language BIP-0039 file: $err")
//    case Right(list) => list
//  }
//
////  private def getMnemonicSize(size: Int) = size match {
////    case 12 => `12`
////    case 15 => `15`
////    case 18 => `18`
////    case 21 => `21`
////    case 24 => `24`
////  }
//
//  property("Generate 32 byte length seeds") {
//    testVectors.foreach { vec =>
//      val mnemonicSize = getMnemonicSize(vec.inputs.mnemonicSize)
//      val mnemonic = Phrase.validated(vec.inputs.mnemonic, mnemonicSize, wordList)
//      val entropy = mnemonic.map(Entropy.unsafeFromPhrase(_, wordList, mnemonicSize))
//
//      curveEntropy.toSeed(entropy.value, Some(vec.inputs.passphrase)) shouldBe vec.outputs.seed32.unsafeStrictBytes(
//        Lengths.`32`
//      )
//    }
//  }
//
//  property("Generate 64 byte length seeds") {
//    testVectors.foreach { vec =>
//      val mnemonicSize = getMnemonicSize(vec.inputs.mnemonicSize)
//      val mnemonic = Phrase.validated(vec.inputs.mnemonic, mnemonicSize, wordList)
//      val entropy = mnemonic.map(Entropy.unsafeFromPhrase(_, wordList, mnemonicSize))
//
//      edEntropy.toSeed(entropy.value, Some(vec.inputs.passphrase)) shouldBe vec.outputs.seed64.unsafeStrictBytes(
//        Lengths.`64`
//      )
//    }
//  }
//
//  property("Generate 96 byte length seeds") {
//    testVectors.foreach { vec =>
//      val mnemonicSize = getMnemonicSize(vec.inputs.mnemonicSize)
//      val mnemonic = Phrase.validated(vec.inputs.mnemonic, mnemonicSize, wordList)
//      val entropy = mnemonic.map(Entropy.unsafeFromPhrase(_, wordList, mnemonicSize))
//
//      extEdEntropy.toSeed(entropy.value, Some(vec.inputs.passphrase)) shouldBe vec.outputs.seed96.unsafeStrictBytes(
//        Lengths.`96`
//      )
//    }
//  }
//}
