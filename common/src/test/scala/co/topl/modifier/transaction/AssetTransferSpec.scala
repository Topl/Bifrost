package co.topl.modifier.transaction

import co.topl.attestation.PublicKeyPropositionCurve25519
import co.topl.modifier.box.{AssetBox, AssetCode, AssetValue}
import co.topl.utils.CommonGenerators
import co.topl.utils.StringDataTypes.Latin1Data
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class AssetTransferSpec
    extends AnyFlatSpec
    with CommonGenerators
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with MockFactory
    with EitherValues {

  val boolGen = Gen.oneOf(true, false)

  "AssetTransfer.validated" should "be invalid if the fee is greater than the poly input" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen) { (firstPolyBox, otherPolyBoxes, sender) =>
      val polyInputs = firstPolyBox +: otherPolyBoxes
      val polyInputsAmount = polyInputs.map(_.value.quantity).sum
      val fee = polyInputsAmount + 1000
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))

      val result = AssetTransfer.validated[PublicKeyPropositionCurve25519](
        polyInputs.map(sender -> _).toIndexedSeq,
        IndexedSeq(),
        IndexedSeq(sender -> AssetValue(100, assetCode)),
        sender,
        sender,
        fee,
        None,
        true
      )

      result shouldBe Symbol("left")
      result.left.value shouldBe AssetTransfer.Validation.InsufficientPolyFunds
    }
  }

  it should "be invalid if there is no poly input" in {
    forAll(addressGen, boolGen) { (sender, minting) =>
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))

      val result = AssetTransfer.validated[PublicKeyPropositionCurve25519](
        IndexedSeq(),
        IndexedSeq(),
        IndexedSeq(sender -> AssetValue(100, assetCode)),
        sender,
        sender,
        0,
        None,
        minting
      )

      result shouldBe Symbol("left")
      result.left.value shouldBe AssetTransfer.Validation.EmptyPolyInputs
    }
  }

  it should "be invalid if there are no asset inputs and not minting" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen) { (firstPolyInput, otherPolyInputs, sender) =>
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))
      val polyInputs = firstPolyInput +: otherPolyInputs

      val result = AssetTransfer.validated[PublicKeyPropositionCurve25519](
        polyInputs.map(sender -> _).toIndexedSeq,
        IndexedSeq(),
        IndexedSeq(sender -> AssetValue(100, assetCode)),
        sender,
        sender,
        0,
        None,
        false
      )

      result shouldBe Symbol("left")
      result.left.value shouldBe AssetTransfer.Validation.EmptyAssetInputs
    }
  }

  it should "be valid if there are no asset inputs and is minting" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen) { (firstPolyInput, otherPolyInputs, sender) =>
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))
      val polyInputs = firstPolyInput +: otherPolyInputs

      val result = AssetTransfer.validated[PublicKeyPropositionCurve25519](
        polyInputs.map(sender -> _).toIndexedSeq,
        IndexedSeq(),
        IndexedSeq(sender -> AssetValue(100, assetCode)),
        sender,
        sender,
        0,
        None,
        true
      )

      result shouldBe Symbol("right")
    }
  }

  it should "be invalid if duplicate poly inputs are used" in {
    forAll(polyBoxGen, addressGen, boolGen) { (polyBox, sender, minting) =>
      val polyInputs = IndexedSeq(polyBox, polyBox)
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))

      val result = AssetTransfer.validated[PublicKeyPropositionCurve25519](
        polyInputs.map(sender -> _),
        IndexedSeq(),
        IndexedSeq(sender -> AssetValue(100, assetCode)),
        sender,
        sender,
        0,
        None,
        minting
      )

      result shouldBe Symbol("left")
      result.left.value shouldBe AssetTransfer.Validation.DuplicatePolyInputs
    }
  }

  it should "be invalid if duplicate asset inputs are used and not minting" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, positiveLongGen) {
      (firstPolyBox, otherPolyBoxes, sender, boxNonce) =>
        val polyInputs = firstPolyBox +: otherPolyBoxes
        val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))
        val assetBox = AssetBox(sender.evidence, boxNonce, AssetValue(100, assetCode))
        val assetBoxes = IndexedSeq(assetBox, assetBox).map(sender -> _)

        val result = AssetTransfer.validated[PublicKeyPropositionCurve25519](
          polyInputs.map(sender -> _).toIndexedSeq,
          assetBoxes,
          IndexedSeq(sender -> AssetValue(100, assetCode)),
          sender,
          sender,
          0,
          None,
          false
        )

        result shouldBe Symbol("left")
        result.left.value shouldBe AssetTransfer.Validation.DuplicateAssetInputs
    }
  }

  it should "be invalid if input asset code is different than output asset code and not minting" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, positiveLongGen) {
      (firstPolyBox, otherPolyBoxes, sender, boxNonce) =>
        val polyInputs = firstPolyBox +: otherPolyBoxes
        val assetCode1 = AssetCode(1.toByte, sender, Latin1Data.unsafe("test1"))
        val assetCode2 = AssetCode(1.toByte, sender, Latin1Data.unsafe("test2"))
        val inputAssetBox = AssetBox(sender.evidence, boxNonce, AssetValue(100, assetCode1))
        val inputAssetBoxes = IndexedSeq(inputAssetBox).map(sender -> _)

        val result = AssetTransfer.validated[PublicKeyPropositionCurve25519](
          polyInputs.map(sender -> _).toIndexedSeq,
          inputAssetBoxes,
          IndexedSeq(sender -> AssetValue(100, assetCode2)),
          sender,
          sender,
          0,
          None,
          false
        )

        result shouldBe Symbol("left")
        result.left.value shouldBe AssetTransfer.Validation.DifferentInputOutputCodes
    }
  }

  it should "be invalid if not enough input assets to pay output and not minting" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, positiveLongGen) {
      (firstPolyBox, otherPolyBoxes, sender, boxNonce) =>
        val polyInputs = firstPolyBox +: otherPolyBoxes
        val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test1"))
        val inputAmount = 100
        val outputAmount = inputAmount + 100
        val inputAssetBox = AssetBox(sender.evidence, boxNonce, AssetValue(inputAmount, assetCode))
        val inputAssetBoxes = IndexedSeq(inputAssetBox).map(sender -> _)

        val result = AssetTransfer.validated[PublicKeyPropositionCurve25519](
          polyInputs.map(sender -> _).toIndexedSeq,
          inputAssetBoxes,
          IndexedSeq(sender -> AssetValue(outputAmount, assetCode)),
          sender,
          sender,
          0,
          None,
          false
        )

        result shouldBe Symbol("left")
        result.left.value shouldBe AssetTransfer.Validation.InsufficientAssetFunds
    }
  }

  it should "be invalid if there are no recipients" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, positiveLongGen, boolGen) {
      (firstPolyBox, otherPolyBoxes, sender, boxNonce, minting) =>
        val polyInputs = firstPolyBox +: otherPolyBoxes
        val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test1"))
        val inputAssetBox = AssetBox(sender.evidence, boxNonce, AssetValue(100, assetCode))
        val inputAssetBoxes = IndexedSeq(inputAssetBox).map(sender -> _)

        val result = AssetTransfer.validated[PublicKeyPropositionCurve25519](
          polyInputs.map(sender -> _).toIndexedSeq,
          inputAssetBoxes,
          IndexedSeq(),
          sender,
          sender,
          0,
          None,
          minting
        )

        result shouldBe Symbol("left")
        result.left.value shouldBe AssetTransfer.Validation.EmptyRecipients
    }
  }

  it should "be invalid if there are duplicate recipients" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, positiveLongGen, boolGen) {
      (firstPolyBox, otherPolyBoxes, sender, boxNonce, minting) =>
        val polyInputs = firstPolyBox +: otherPolyBoxes
        val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test1"))
        val inputAssetBox = AssetBox(sender.evidence, boxNonce, AssetValue(100, assetCode))
        val inputAssetBoxes = IndexedSeq(inputAssetBox).map(sender -> _)
        val recipients = IndexedSeq(
          sender -> AssetValue(1, assetCode),
          sender -> AssetValue(1, assetCode)
        )

        val result = AssetTransfer.validated[PublicKeyPropositionCurve25519](
          polyInputs.map(sender -> _).toIndexedSeq,
          inputAssetBoxes,
          recipients,
          sender,
          sender,
          0,
          None,
          minting
        )

        result shouldBe Symbol("left")
        result.left.value shouldBe AssetTransfer.Validation.DuplicateRecipients
    }
  }

  it should "be invalid if multiple asset codes are used as inputs" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, positiveLongGen) {
      (firstPolyBox, otherPolyBoxes, sender, boxNonce) =>
        val polyInputs = firstPolyBox +: otherPolyBoxes
        val assetCode1 = AssetCode(1.toByte, sender, Latin1Data.unsafe("test1"))
        val assetCode2 = AssetCode(1.toByte, sender, Latin1Data.unsafe("test2"))
        val inputAssetBoxes =
          IndexedSeq(
            sender -> AssetBox(sender.evidence, boxNonce, AssetValue(100, assetCode1)),
            sender -> AssetBox(sender.evidence, boxNonce + 1, AssetValue(100, assetCode2))
          )

        val result = AssetTransfer.validated[PublicKeyPropositionCurve25519](
          polyInputs.map(sender -> _).toIndexedSeq,
          inputAssetBoxes,
          IndexedSeq(sender -> AssetValue(10, assetCode1)),
          sender,
          sender,
          0,
          None,
          false
        )

        result shouldBe Symbol("left")
        result.left.value shouldBe AssetTransfer.Validation.MultipleAssetCodes
    }
  }
}
