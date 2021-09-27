package co.topl.modifier.transaction.builder

import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.modifier.BoxReader
import co.topl.modifier.box.{AssetBox, AssetCode, AssetValue, ProgramId}
import co.topl.utils.CommonGenerators
import co.topl.utils.StringDataTypes.Latin1Data
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class BuildUnsignedAssetTransferSpec
    extends AnyFlatSpec
    with CommonGenerators
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with MockFactory
    with EitherValues {

  val boolGen = Gen.oneOf(true, false)

  "buildUnsignedAssetTransfer" should "return invalid if the fee is greater than the poly input" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen) { (firstFeeBox, otherFeeBoxes, sender) =>
      val polyBoxInputs = firstFeeBox :: otherFeeBoxes
      val fee = polyBoxInputs.map(_.value.quantity).sum + 100
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))
      val senders = List(sender)
      val recipients = List(sender -> AssetValue(10, assetCode))

      val boxReader = mock[BoxReader[ProgramId, Address]]
      (boxReader.getTokenBoxes _).expects(sender).returns(Some(polyBoxInputs))

      val request = TransferRequests.AssetTransferRequest(
        senders,
        recipients,
        sender,
        sender,
        fee,
        None,
        true
      )

      val result =
        TransferBuilder
          .buildUnsignedAssetTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result shouldBe Symbol("left")
      result.left.value shouldBe BuildTransferFailures.InsufficientFeeFunds
    }
  }

  it should "return invalid if there are no poly inputs" in {
    forAll(addressGen, boolGen) { (sender, minting) =>
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))
      val senders = List(sender)
      val recipients = List(sender -> AssetValue(10, assetCode))

      val boxReader = mock[BoxReader[ProgramId, Address]]
      (boxReader.getTokenBoxes _).expects(sender).returns(Some(List()))

      val request = TransferRequests.AssetTransferRequest(
        senders,
        recipients,
        sender,
        sender,
        0,
        None,
        minting
      )

      val result =
        TransferBuilder
          .buildUnsignedAssetTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result shouldBe Symbol("left")
      result.left.value shouldBe BuildTransferFailures.EmptyInputs
    }
  }

  it should "return invalid if there are no asset inputs and not minting" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen) { (firstPolyInput, otherPolyInputs, sender) =>
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))
      val polyInputs = firstPolyInput :: otherPolyInputs
      val senders = List(sender)
      val recipients = List(sender -> AssetValue(10, assetCode))

      val boxReader = mock[BoxReader[ProgramId, Address]]
      (boxReader.getTokenBoxes _).expects(sender).returns(Some(polyInputs))

      val request = TransferRequests.AssetTransferRequest(
        senders,
        recipients,
        sender,
        sender,
        0,
        None,
        false
      )

      val result =
        TransferBuilder
          .buildUnsignedAssetTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result shouldBe Symbol("left")
      result.left.value shouldBe BuildTransferFailures.EmptyInputs
    }
  }

  it should "return valid if there are no asset inputs and is minting" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen) { (firstPolyInput, otherPolyInputs, sender) =>
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))
      val polyInputs = firstPolyInput :: otherPolyInputs
      val senders = List(sender)
      val recipients = List(sender -> AssetValue(100, assetCode))

      val boxReader = mock[BoxReader[ProgramId, Address]]
      (boxReader.getTokenBoxes _).expects(sender).returns(Some(polyInputs))

      val request = TransferRequests.AssetTransferRequest(
        senders,
        recipients,
        sender,
        sender,
        0,
        None,
        true
      )

      val result =
        TransferBuilder
          .buildUnsignedAssetTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result shouldBe Symbol("right")
    }
  }

  it should "return invalid if duplicate poly inputs are used" in {
    forAll(polyBoxGen, addressGen, boolGen) { (polyBox, sender, minting) =>
      val polyInputs = List(polyBox)
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))
      val senders = List(sender)
      val recipients = List(sender -> AssetValue(100, assetCode))

      val boxReader = mock[BoxReader[ProgramId, Address]]
      (boxReader.getTokenBoxes _).expects(sender).returns(Some(polyInputs ++ polyInputs))

      val request = TransferRequests.AssetTransferRequest(
        senders,
        recipients,
        sender,
        sender,
        0,
        None,
        minting
      )

      val result =
        TransferBuilder
          .buildUnsignedAssetTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result shouldBe Symbol("left")
      result.left.value shouldBe BuildTransferFailures.DuplicateInputs
    }
  }

  it should "return invalid if duplicate asset inputs are used and not minting" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, positiveLongGen) {
      (firstPolyBox, otherPolyBoxes, sender, boxNonce) =>
        val polyInputs = firstPolyBox :: otherPolyBoxes
        val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))
        val assetBox = AssetBox(sender.evidence, boxNonce, AssetValue(100, assetCode))
        val assetBoxes = List(assetBox, assetBox)
        val senders = List(sender)
        val recipients = List(sender -> AssetValue(100, assetCode))

        val boxReader = mock[BoxReader[ProgramId, Address]]
        (boxReader.getTokenBoxes _).expects(sender).returns(Some(polyInputs ++ assetBoxes))

        val request = TransferRequests.AssetTransferRequest(
          senders,
          recipients,
          sender,
          sender,
          0,
          None,
          false
        )

        val result =
          TransferBuilder
            .buildUnsignedAssetTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

        result shouldBe Symbol("left")
        result.left.value shouldBe BuildTransferFailures.DuplicateInputs
    }
  }

  it should "return invalid if input asset code is different than output asset code and not minting" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, positiveLongGen) {
      (firstPolyBox, otherPolyBoxes, sender, boxNonce) =>
        val polyInputs = firstPolyBox :: otherPolyBoxes
        val assetCode1 = AssetCode(1.toByte, sender, Latin1Data.unsafe("test1"))
        val assetCode2 = AssetCode(1.toByte, sender, Latin1Data.unsafe("test2"))
        val inputAssetBox = AssetBox(sender.evidence, boxNonce, AssetValue(100, assetCode1))
        val inputAssetBoxes = List(inputAssetBox)
        val senders = List(sender)
        val recipients = List(sender -> AssetValue(100, assetCode2))

        val boxReader = mock[BoxReader[ProgramId, Address]]
        (boxReader.getTokenBoxes _).expects(sender).returns(Some(polyInputs ++ inputAssetBoxes))

        val request = TransferRequests.AssetTransferRequest(
          senders,
          recipients,
          sender,
          sender,
          0,
          None,
          false
        )

        val result =
          TransferBuilder
            .buildUnsignedAssetTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

        result shouldBe Symbol("left")
        // non-matching input assets will be filtered out by box selection algorithm
        result.left.value shouldBe BuildTransferFailures.EmptyInputs
    }
  }

  it should "return invalid if not enough input assets to pay output and not minting" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, positiveLongGen) {
      (firstPolyBox, otherPolyBoxes, sender, boxNonce) =>
        val polyInputs = firstPolyBox :: otherPolyBoxes
        val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test1"))
        val inputAmount = 100
        val outputAmount = inputAmount + 100
        val inputAssetBox = AssetBox(sender.evidence, boxNonce, AssetValue(inputAmount, assetCode))
        val inputAssetBoxes = List(inputAssetBox)
        val senders = List(sender)
        val recipients = List(sender -> AssetValue(outputAmount, assetCode))

        val boxReader = mock[BoxReader[ProgramId, Address]]
        (boxReader.getTokenBoxes _).expects(sender).returns(Some(polyInputs ++ inputAssetBoxes))

        val request = TransferRequests.AssetTransferRequest(
          senders,
          recipients,
          sender,
          sender,
          0,
          None,
          false
        )

        val result = TransferBuilder
          .buildUnsignedAssetTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

        result shouldBe Symbol("left")
        result.left.value shouldBe BuildTransferFailures.InsufficientPaymentFunds
    }
  }

  it should "return invalid if there are no recipients" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, positiveLongGen) {
      (firstPolyBox, otherPolyBoxes, sender, boxNonce) =>
        val polyInputs = firstPolyBox :: otherPolyBoxes
        val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test1"))
        val inputAssetBox = AssetBox(sender.evidence, boxNonce, AssetValue(100, assetCode))
        val inputAssetBoxes = List(inputAssetBox)
        val senders = List(sender)
        val recipients = List()

        val boxReader = mock[BoxReader[ProgramId, Address]]
        (boxReader.getTokenBoxes _).expects(sender).returns(Some(polyInputs ++ inputAssetBoxes))

        val request = TransferRequests.AssetTransferRequest(
          senders,
          recipients,
          sender,
          sender,
          0,
          None,
          true
        )

        val result = TransferBuilder
          .buildUnsignedAssetTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

        result shouldBe Symbol("left")
        // input assets will be filtered out because there are no matching asset codes to send to
        result.left.value shouldBe BuildTransferFailures.EmptyRecipients
    }
  }

  it should "return invalid if there are duplicate recipients" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, positiveLongGen, boolGen) {
      (firstPolyBox, otherPolyBoxes, sender, boxNonce, minting) =>
        val polyInputs = firstPolyBox :: otherPolyBoxes
        val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test1"))
        val inputAssetBox = AssetBox(sender.evidence, boxNonce, AssetValue(100, assetCode))
        val inputAssetBoxes = List(inputAssetBox)
        val senders = List(sender)
        val recipients = List(
          sender -> AssetValue(1, assetCode),
          sender -> AssetValue(1, assetCode)
        )

        val boxReader = mock[BoxReader[ProgramId, Address]]
        (boxReader.getTokenBoxes _).expects(sender).returns(Some(polyInputs ++ inputAssetBoxes))

        val request = TransferRequests.AssetTransferRequest(
          senders,
          recipients,
          sender,
          sender,
          0,
          None,
          minting
        )

        val result = TransferBuilder
          .buildUnsignedAssetTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

        result shouldBe Symbol("left")
        result.left.value shouldBe BuildTransferFailures.DuplicateRecipients
    }
  }
}
