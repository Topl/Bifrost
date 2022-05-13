package co.topl.modifier.transaction.builder

import cats.data.Chain
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.modifier.box.{AssetBox, AssetCode, AssetValue}
import co.topl.modifier.{BoxReader, ProgramId}
import co.topl.utils.CommonGenerators
import co.topl.utils.StringDataTypes.Latin1Data
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import co.topl.modifier.transaction.builder.Generators._

class BuildUnsignedAssetTransferSpec
    extends AnyFlatSpec
    with CommonGenerators
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with MockFactory
    with EitherValues {

  val boolGen: Gen[Boolean] = Gen.oneOf(true, false)

  behavior of "buildUnsignedAssetTransfer"

  it should "return invalid if the fee is greater than the polys provided" in {
    forAll(polyBoxesGen, addressGen) { (polyBoxes, sender) =>
      val existingPolys = boxesAmount(polyBoxes)

      val fee = existingPolys + 100
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))
      val senders = List(sender)
      val recipients = List(sender -> AssetValue(10, assetCode))

      val boxReader = MockBoxReader.fromNec(sender -> polyBoxes)

      val expectedFailure =
        BuildTransferFailures.InsufficientPolyFunds(
          existingPolys,
          fee
        )

      val request = TransferRequests.AssetTransferRequest(
        senders,
        recipients,
        sender,
        sender,
        fee,
        None,
        minting = true
      )

      val result =
        TransferBuilder
          .buildUnsignedAssetTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result.left.value shouldBe expectedFailure
    }
  }

  it should "return invalid if there are no asset inputs and not minting" in {
    forAll(polyBoxesGen, addressGen) { (polyBoxes, sender) =>
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))
      val senders = List(sender)
      val assetsToSend = 10
      val recipients = List(sender -> AssetValue(assetsToSend, assetCode))

      val boxReader = MockBoxReader.fromNec(sender -> polyBoxes)

      val request = TransferRequests.AssetTransferRequest(
        senders,
        recipients,
        sender,
        sender,
        0,
        None,
        minting = false
      )

      val result =
        TransferBuilder
          .buildUnsignedAssetTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result.left.value shouldBe BuildTransferFailures.InsufficientAssetFunds(assetCode, 0, assetsToSend)
    }
  }

  it should "return valid if there are no asset inputs and is minting" in {
    forAll(polyBoxesGen, addressGen) { (polyBoxes, sender) =>
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))
      val senders = List(sender)
      val recipients = List(sender -> AssetValue(100, assetCode))

      val boxReader = MockBoxReader.fromNec(sender -> polyBoxes)

      val request = TransferRequests.AssetTransferRequest(
        senders,
        recipients,
        sender,
        sender,
        0,
        None,
        minting = true
      )

      val result =
        TransferBuilder
          .buildUnsignedAssetTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result shouldBe Symbol("right")
    }
  }

  it should "return invalid if duplicate poly inputs are used" in {
    forAll(polyBoxGen, addressGen, boolGen) { (polyBox, sender, minting) =>
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))
      val senders = List(sender)
      val recipients = List(sender -> AssetValue(100, assetCode))

      // provide the same poly box twice
      val boxReader = MockBoxReader.fromSeq(sender -> List(polyBox, polyBox))

      val request =
        TransferRequests.AssetTransferRequest(
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

      result.left.value shouldBe BuildTransferFailures.DuplicateInputs
    }
  }

  it should "return invalid if duplicate asset inputs are used and not minting" in {
    forAll(polyBoxesGen, addressGen, positiveLongGen) { (polyBoxes, sender, boxNonce) =>
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test"))
      val assetBox = AssetBox(sender.evidence, boxNonce, AssetValue(100, assetCode))
      val senders = List(sender)
      val recipients = List(sender -> AssetValue(100, assetCode))

      val boxReader = MockBoxReader.fromNec(sender -> polyBoxes.appendChain(Chain(assetBox, assetBox)))

      val request = TransferRequests.AssetTransferRequest(
        senders,
        recipients,
        sender,
        sender,
        0,
        None,
        minting = false
      )

      val result =
        TransferBuilder
          .buildUnsignedAssetTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result.left.value shouldBe BuildTransferFailures.DuplicateInputs
    }
  }

  it should "return invalid if input asset code is different than output asset code and not minting" in {
    forAll(polyBoxesGen, addressGen, positiveLongGen) { (polyBoxes, sender, boxNonce) =>
      val assetCode1 = AssetCode(1.toByte, sender, Latin1Data.unsafe("test1"))
      val assetCode2 = AssetCode(1.toByte, sender, Latin1Data.unsafe("test2"))

      val inputAssetBox = AssetBox(sender.evidence, boxNonce, AssetValue(100, assetCode1))

      val assetsToSend = 100
      val senders = List(sender)
      val recipients = List(sender -> AssetValue(assetsToSend, assetCode2))

      val boxReader = MockBoxReader.fromNec(sender -> polyBoxes.append(inputAssetBox))

      val request = TransferRequests.AssetTransferRequest(
        senders,
        recipients,
        sender,
        sender,
        0,
        None,
        minting = false
      )

      val result =
        TransferBuilder
          .buildUnsignedAssetTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      // non-matching input assets will be filtered out by the box selection algorithm
      result.left.value shouldBe BuildTransferFailures.InsufficientAssetFunds(assetCode2, 0, assetsToSend)
    }
  }

  it should "return invalid if not enough input assets to pay output and not minting" in {
    forAll(polyBoxesGen, addressGen, positiveLongGen) { (polyBoxes, sender, boxNonce) =>
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test1"))

      val inputAmount = 100
      val outputAmount = inputAmount + 100

      val inputAssetBox = AssetBox(sender.evidence, boxNonce, AssetValue(inputAmount, assetCode))

      val senders = List(sender)
      val recipients = List(sender -> AssetValue(outputAmount, assetCode))

      val boxReader = MockBoxReader.fromNec(sender -> polyBoxes.append(inputAssetBox))

      val request = TransferRequests.AssetTransferRequest(
        senders,
        recipients,
        sender,
        sender,
        0,
        None,
        minting = false
      )

      val result = TransferBuilder
        .buildUnsignedAssetTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result.left.value shouldBe BuildTransferFailures.InsufficientAssetFunds(assetCode, inputAmount, outputAmount)
    }
  }

  it should "return invalid if there are no recipients" in {
    forAll(polyBoxesGen, addressGen, positiveLongGen) { (polyBoxes, sender, boxNonce) =>
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test1"))
      val inputAssetBox = AssetBox(sender.evidence, boxNonce, AssetValue(100, assetCode))
      val senders = List(sender)
      val recipients = List()

      val boxReader = MockBoxReader.fromNec(sender -> polyBoxes.append(inputAssetBox))

      val request = TransferRequests.AssetTransferRequest(
        senders,
        recipients,
        sender,
        sender,
        0,
        None,
        minting = true
      )

      val result = TransferBuilder
        .buildUnsignedAssetTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result.left.value shouldBe BuildTransferFailures.EmptyOutputs
    }
  }

  it should "return invalid if there are duplicate recipients" in {
    forAll(polyBoxesGen, addressGen, positiveLongGen, boolGen) { (polyBoxes, sender, boxNonce, minting) =>
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("test1"))

      val inputAssetBox = AssetBox(sender.evidence, boxNonce, AssetValue(100, assetCode))

      val senders = List(sender)
      val recipients = List(
        sender -> AssetValue(1, assetCode),
        sender -> AssetValue(10, assetCode)
      )

      val boxReader = MockBoxReader.fromNec(sender -> polyBoxes.append(inputAssetBox))

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

      result.left.value shouldBe BuildTransferFailures.DuplicateOutputs
    }
  }

  it should "return a valid transfer with no asset inputs when minting" in {
    forAll(polyBoxesGen, addressGen) { (polyBoxes, sender) =>
      val assetCode = AssetCode(1.toByte, sender, Latin1Data.unsafe("asset"))

      val existingAssetNonce = 8000

      val existingAssetBox = AssetBox(sender.evidence, existingAssetNonce, AssetValue(100, assetCode))

      val boxReader = MockBoxReader.fromNec(sender -> polyBoxes.append(existingAssetBox))

      val request = TransferRequests.AssetTransferRequest(
        List(sender),
        List(sender -> AssetValue(6000, assetCode)),
        sender,
        sender,
        0,
        None,
        minting = true
      )

      val result = TransferBuilder
        .buildUnsignedAssetTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result.value.from.map(_._2) shouldNot contain(existingAssetNonce)
    }
  }
}
