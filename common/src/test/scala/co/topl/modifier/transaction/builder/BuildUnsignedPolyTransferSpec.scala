package co.topl.modifier.transaction.builder

import cats.implicits._
import cats.data.NonEmptyChain
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.modifier.box.{PolyBox, SimpleValue}
import co.topl.modifier.{BoxReader, ProgramId}
import co.topl.utils.{CommonGenerators, Int128}
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import co.topl.modifier.transaction.builder.Generators._

class BuildUnsignedPolyTransferSpec
    extends AnyFlatSpec
    with CommonGenerators
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with MockFactory
    with EitherValues {

  behavior of "buildUnsignedPolyTransfer"

  it should "return invalid if not enough funds for fee" in {
    forAll(polyBoxesGen, addressGen, addressGen) { (polyBoxes, sender, recipient) =>
      val existingPolys = boxesAmount(polyBoxes)

      val fee = existingPolys + 100000

      val polysToSend = 100

      val boxReader = MockBoxReader.fromNec(sender -> polyBoxes)

      val expectedFailure =
        BuildTransferFailures.InsufficientPolyFunds(
          existingPolys,
          polysToSend + fee
        )

      val request = TransferRequests.PolyTransferRequest(
        List(sender),
        List(recipient -> polysToSend),
        sender,
        fee,
        None
      )

      val result = TransferBuilder
        .buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result.left.value shouldBe expectedFailure
    }
  }

  it should "return invalid if not enough funds for recipients" in {
    forAll(polyBoxesGen, addressGen, addressGen) { (polyBoxes, sender, recipient) =>
      val existingPolys = boxesAmount(polyBoxes)
      val paymentAmount: Int128 = existingPolys + 100

      val boxReader = MockBoxReader.fromNec(sender -> polyBoxes)

      val request = TransferRequests.PolyTransferRequest(
        List(sender),
        List(recipient -> paymentAmount),
        sender,
        0,
        None
      )

      val result =
        TransferBuilder
          .buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result.left.value shouldBe BuildTransferFailures.InsufficientPolyFunds(existingPolys, paymentAmount)
    }
  }

  it should "return invalid if no poly boxes provided" in {
    forAll(addressGen, addressGen) { (sender, recipient) =>
      val boxReader = MockBoxReader.empty

      val polysToSend = 100

      val request = TransferRequests.PolyTransferRequest(
        List(sender),
        List(recipient -> polysToSend),
        sender,
        0,
        None
      )

      val result =
        TransferBuilder
          .buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result.left.value shouldBe BuildTransferFailures.EmptyPolyInputs
    }
  }

  it should "return invalid if duplicate input boxes" in {
    forAll(polyBoxGen, addressGen, addressGen) { (box, sender, recipient) =>
      val boxReader = MockBoxReader.fromSeq(sender -> List(box, box))

      val request = TransferRequests.PolyTransferRequest(
        List(sender),
        List(recipient -> 100),
        sender,
        0,
        None
      )

      val result =
        TransferBuilder
          .buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result.left.value shouldBe BuildTransferFailures.DuplicateInputs
    }
  }

  it should "return invalid if no recipients" in {
    forAll(polyBoxesGen, addressGen) { (polyBoxes, sender) =>
      val boxReader = MockBoxReader.fromNec(sender -> polyBoxes)

      val request = TransferRequests.PolyTransferRequest(
        List(sender),
        List(),
        sender,
        0,
        None
      )

      val result =
        TransferBuilder
          .buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result.left.value shouldBe BuildTransferFailures.EmptyOutputs
    }
  }

  it should "return invalid if duplicate recipients" in {
    forAll(polyBoxesGen, addressGen, addressGen) { (polyBoxes, sender, recipient) =>
      val boxReader = MockBoxReader.fromNec(sender -> polyBoxes)

      val request = TransferRequests.PolyTransferRequest(
        List(sender),
        List(recipient -> 0, recipient -> 0),
        sender,
        0,
        None
      )

      val result =
        TransferBuilder
          .buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result.left.value shouldBe BuildTransferFailures.DuplicateOutputs
    }
  }

  it should "return valid if valid inputs" in {
    forAll(polyBoxesGen, addressGen, addressGen) { (polyBoxes, sender, recipient) =>
      val boxReader = MockBoxReader.fromNec(sender -> polyBoxes)

      val request = TransferRequests.PolyTransferRequest(
        List(sender),
        List(recipient -> 1),
        sender,
        1,
        None
      )

      val result =
        TransferBuilder
          .buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result shouldBe Symbol("right")
    }
  }
}
