package co.topl.modifier.transaction.unsigned

import co.topl.attestation.PublicKeyPropositionCurve25519
import co.topl.modifier.box.{AssetBox, AssetCode, AssetValue}
import co.topl.modifier.transaction.AssetTransfer
import co.topl.utils.CommonGenerators
import co.topl.utils.StringDataTypes.Latin1Data
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class AssetTransferRequestSpec
    extends AnyFlatSpec
    with CommonGenerators
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with MockFactory
    with EitherValues {

  val boolGen = Gen.oneOf(true, false)

  "unsignedTxFromAssetTransferRequest" should "be invalid if the fee is greater than the poly input" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, boolGen) { (firstFeeBox, otherFeeBoxes, sender, minting) =>
      val polyBoxInputs = otherFeeBoxes.prepended(firstFeeBox).map(sender -> _)
      val fee = polyBoxInputs.map(_._2.value.quantity).sum + 100
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))
      val senders = List(sender)
      val recipients = List(sender -> AssetValue(10, assetCode))

      val request = TransferRequests.AssetTransferRequest(
        senders,
        recipients,
        sender,
        sender,
        fee,
        None,
        minting,
        PropositionTypes.PublicKeyEd25519
      )

      val result = fromAssetTransferRequest(request, polyBoxInputs, List())

      result shouldBe Symbol("left")
      result.left.value shouldBe UnsignedTransferFailures.InsufficientFeeFunds
    }
  }

  it should "be invalid if there are no poly inputs" in {
    forAll(addressGen, boolGen) { (sender, minting) =>
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))
      val senders = List(sender)
      val recipients = List(sender -> AssetValue(10, assetCode))

      val request = TransferRequests.AssetTransferRequest(
        senders,
        recipients,
        sender,
        sender,
        0,
        None,
        minting,
        PropositionTypes.PublicKeyEd25519
      )

      val result = fromAssetTransferRequest(request, List(), List())

      result shouldBe Symbol("left")
      result.left.value shouldBe UnsignedTransferFailures.EmptyInputs
    }
  }

  it should "be invalid if there are no asset inputs and not minting" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen) { (firstPolyInput, otherPolyInputs, sender) =>
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))
      val polyInputs = otherPolyInputs.prepended(firstPolyInput).map(sender -> _)
      val senders = List(sender)
      val recipients = List(sender -> AssetValue(10, assetCode))

      val request = TransferRequests.AssetTransferRequest(
        senders,
        recipients,
        sender,
        sender,
        0,
        None,
        false,
        PropositionTypes.PublicKeyEd25519
      )

      val result = fromAssetTransferRequest(request, polyInputs, List())

      result shouldBe Symbol("left")
      result.left.value shouldBe UnsignedTransferFailures.EmptyInputs
    }
  }

  it should "be valid if there are no asset inputs and is minting" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen) { (firstPolyInput, otherPolyInputs, sender) =>
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))
      val polyInputs = otherPolyInputs.prepended(firstPolyInput).map(sender -> _)
      val senders = List(sender)
      val recipients = List(sender -> AssetValue(100, assetCode))

      val request = TransferRequests.AssetTransferRequest(
        senders,
        recipients,
        sender,
        sender,
        0,
        None,
        true,
        PropositionTypes.PublicKeyEd25519
      )

      val result = fromAssetTransferRequest(request, polyInputs, List())

      result shouldBe Symbol("right")
    }
  }

  it should "be invalid if duplicate poly inputs are used" in {
    forAll(polyBoxGen, addressGen, boolGen) { (polyBox, sender, minting) =>
      val polyInputs = List(polyBox).map(sender -> _)
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))
      val senders = List(sender)
      val recipients = List(sender -> AssetValue(100, assetCode))

      val request = TransferRequests.AssetTransferRequest(
        senders,
        recipients,
        sender,
        sender,
        0,
        None,
        minting,
        PropositionTypes.PublicKeyEd25519
      )

      val result = fromAssetTransferRequest(request, polyInputs ++ polyInputs, List())

      result shouldBe Symbol("left")
      result.left.value shouldBe UnsignedTransferFailures.DuplicateInputs
    }
  }

  it should "be invalid if duplicate asset inputs are used and not minting" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, positiveLongGen) {
      (firstPolyBox, otherPolyBoxes, sender, boxNonce) =>
        val polyInputs = otherPolyBoxes.prepended(firstPolyBox).map(sender -> _)
        val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))
        val assetBox = AssetBox(sender.evidence, boxNonce, AssetValue(100, assetCode))
        val assetBoxes = List(assetBox, assetBox).map(sender -> _)
        val senders = List(sender)
        val recipients = List(sender -> AssetValue(100, assetCode))

        val request = TransferRequests.AssetTransferRequest(
          senders,
          recipients,
          sender,
          sender,
          0,
          None,
          false,
          PropositionTypes.PublicKeyEd25519
        )

        val result = fromAssetTransferRequest(request, polyInputs, assetBoxes)

        result shouldBe Symbol("left")
        result.left.value shouldBe UnsignedTransferFailures.DuplicateInputs
    }
  }

  it should "be invalid if input asset code is different than output asset code and not minting" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, positiveLongGen) {
      (firstPolyBox, otherPolyBoxes, sender, boxNonce) =>
        val polyInputs = otherPolyBoxes.prepended(firstPolyBox).map(sender -> _)
        val assetCode1 = AssetCode(1.toByte, sender, Latin1Data.unsafe("test1"))
        val assetCode2 = AssetCode(1.toByte, sender, Latin1Data.unsafe("test2"))
        val inputAssetBox = AssetBox(sender.evidence, boxNonce, AssetValue(100, assetCode1))
        val inputAssetBoxes = List(inputAssetBox).map(sender -> _)
        val senders = List(sender)
        val recipients = List(sender -> AssetValue(100, assetCode2))

        val request = TransferRequests.AssetTransferRequest(
          senders,
          recipients,
          sender,
          sender,
          0,
          None,
          false,
          PropositionTypes.PublicKeyEd25519
        )

        val result = fromAssetTransferRequest(request, polyInputs, inputAssetBoxes)

        result shouldBe Symbol("left")
        result.left.value shouldBe UnsignedTransferFailures.DifferentInputOutputCodes
    }
  }

  it should "be invalid if not enough input assets to pay output and not minting" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, positiveLongGen) {
      (firstPolyBox, otherPolyBoxes, sender, boxNonce) =>
        val polyInputs = otherPolyBoxes.prepended(firstPolyBox).map(sender -> _)
        val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test1"))
        val inputAmount = 100
        val outputAmount = inputAmount + 100
        val inputAssetBox = AssetBox(sender.evidence, boxNonce, AssetValue(inputAmount, assetCode))
        val inputAssetBoxes = List(inputAssetBox).map(sender -> _)
        val senders = List(sender)
        val recipients = List(sender -> AssetValue(outputAmount, assetCode))

        val request = TransferRequests.AssetTransferRequest(
          senders,
          recipients,
          sender,
          sender,
          0,
          None,
          false,
          PropositionTypes.PublicKeyEd25519
        )

        val result = fromAssetTransferRequest(request, polyInputs, inputAssetBoxes)

        result shouldBe Symbol("left")
        result.left.value shouldBe UnsignedTransferFailures.InsufficientPaymentFunds
    }
  }

  it should "be invalid if there are no recipients" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, positiveLongGen, boolGen) {
      (firstPolyBox, otherPolyBoxes, sender, boxNonce, minting) =>
        val polyInputs = otherPolyBoxes.prepended(firstPolyBox).map(sender -> _)
        val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test1"))
        val inputAssetBox = AssetBox(sender.evidence, boxNonce, AssetValue(100, assetCode))
        val inputAssetBoxes = List(inputAssetBox).map(sender -> _)
        val senders = List(sender)
        val recipients = List()

        val request = TransferRequests.AssetTransferRequest(
          senders,
          recipients,
          sender,
          sender,
          0,
          None,
          false,
          PropositionTypes.PublicKeyEd25519
        )

        val result = fromAssetTransferRequest(request, polyInputs, inputAssetBoxes)

        result shouldBe Symbol("left")
        result.left.value shouldBe UnsignedTransferFailures.EmptyRecipients
    }
  }

  it should "be invalid if there are duplicate recipients" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, positiveLongGen, boolGen) {
      (firstPolyBox, otherPolyBoxes, sender, boxNonce, minting) =>
        val polyInputs = otherPolyBoxes.prepended(firstPolyBox).map(sender -> _)
        val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test1"))
        val inputAssetBox = AssetBox(sender.evidence, boxNonce, AssetValue(100, assetCode))
        val inputAssetBoxes = List(inputAssetBox).map(sender -> _)
        val senders = List(sender)
        val recipients = List(
          sender -> AssetValue(1, assetCode),
          sender -> AssetValue(1, assetCode)
        )

        val request = TransferRequests.AssetTransferRequest(
          senders,
          recipients,
          sender,
          sender,
          0,
          None,
          false,
          PropositionTypes.PublicKeyEd25519
        )

        val result = fromAssetTransferRequest(request, polyInputs, inputAssetBoxes)

        result shouldBe Symbol("left")
        result.left.value shouldBe UnsignedTransferFailures.DuplicateRecipients
    }
  }

  it should "be invalid if multiple asset codes are used as inputs" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, positiveLongGen) {
      (firstPolyBox, otherPolyBoxes, sender, boxNonce) =>
        val polyInputs = otherPolyBoxes.prepended(firstPolyBox).map(sender -> _)
        val assetCode1 = AssetCode(1.toByte, sender, Latin1Data.unsafe("test1"))
        val assetCode2 = AssetCode(1.toByte, sender, Latin1Data.unsafe("test2"))
        val inputAssetBoxes =
          List(
            sender -> AssetBox(sender.evidence, boxNonce, AssetValue(100, assetCode1)),
            sender -> AssetBox(sender.evidence, boxNonce + 1, AssetValue(100, assetCode2))
          )
        val senders = List(sender)
        val recipients = List(sender -> AssetValue(10, assetCode1))

        val request = TransferRequests.AssetTransferRequest(
          senders,
          recipients,
          sender,
          sender,
          0,
          None,
          false,
          PropositionTypes.PublicKeyEd25519
        )

        val result = fromAssetTransferRequest(request, polyInputs, inputAssetBoxes)

        result shouldBe Symbol("left")
        result.left.value shouldBe UnsignedTransferFailures.EmptyRecipients
    }
  }
}
