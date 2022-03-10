package co.topl.modifier.transaction.builder

import co.topl.attestation.PublicKeyPropositionCurve25519
import co.topl.modifier.transaction.builder.Generators._
import co.topl.utils.CommonGenerators
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class BuildUnsignedArbitTransferSpec
    extends AnyFlatSpec
    with CommonGenerators
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with EitherValues {

  "buildUnsignedArbitTransfer" should "return invalid if not enough funds for fee" in {
    forAll(polyBoxesGen, arbitBoxesGen, addressGen, addressGen) { (polyBoxes, arbitBoxes, sender, recipient) =>
      val existingPolys = boxesAmount(polyBoxes)
      val fee = existingPolys + 100000

      val boxReader = MockBoxReader.fromNec(sender -> (polyBoxes ++ arbitBoxes))

      val expectedFailure =
        BuildTransferFailures.InsufficientPolyFunds(
          existingPolys,
          fee
        )

      val request = TransferRequests.ArbitTransferRequest(
        List(sender),
        List(recipient -> 100),
        sender,
        sender,
        fee,
        None
      )

      val result = TransferBuilder
        .buildUnsignedArbitTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result.left.value shouldBe expectedFailure
    }
  }

  it should "return invalid if not enough funds for recipients" in {
    forAll(polyBoxesGen, arbitBoxesGen, addressGen, addressGen) { (polyBoxes, arbitBoxes, sender, recipient) =>
      val existingArbits = boxesAmount(arbitBoxes)
      val toSend = existingArbits + 10000

      val boxReader = MockBoxReader.fromNec(sender -> (polyBoxes ++ arbitBoxes))

      val request = TransferRequests.ArbitTransferRequest(
        List(sender),
        List(recipient -> toSend),
        sender,
        sender,
        0,
        None
      )

      val result = TransferBuilder
        .buildUnsignedArbitTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result.left.value shouldBe BuildTransferFailures.InsufficientArbitFunds(existingArbits, toSend)
    }
  }

  it should "return invalid if no input arbit boxes" in {
    forAll(polyBoxesGen, addressGen, addressGen) { (polyBoxes, sender, recipient) =>
      val boxReader = MockBoxReader.fromNec(sender -> polyBoxes)

      val arbitsToSend = 1

      val request = TransferRequests.ArbitTransferRequest(
        List(sender),
        List(recipient -> arbitsToSend),
        sender,
        sender,
        0,
        None
      )

      val result = TransferBuilder
        .buildUnsignedArbitTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result.left.value shouldBe BuildTransferFailures.InsufficientArbitFunds(0, arbitsToSend)
    }
  }

  it should "return invalid if duplicate poly input boxes" in {
    forAll(polyBoxesGen, arbitBoxesGen, addressGen, addressGen) { (polyBoxes, arbitBoxes, sender, recipient) =>
      // provide poly boxes twice
      val boxReader = MockBoxReader.fromNec(sender -> (polyBoxes ++ polyBoxes ++ arbitBoxes))

      val request = TransferRequests.ArbitTransferRequest(
        List(sender),
        List(recipient -> 100),
        sender,
        sender,
        0,
        None
      )

      val result = TransferBuilder
        .buildUnsignedArbitTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result.left.value shouldBe BuildTransferFailures.DuplicateInputs
    }
  }

  it should "return invalid if duplicate arbit input boxes" in {
    forAll(polyBoxesGen, arbitBoxesGen, addressGen, addressGen) { (polyBoxes, arbitBoxes, sender, recipient) =>
      // provide arbit boxes twice
      val boxReader = MockBoxReader.fromNec(sender -> (polyBoxes ++ arbitBoxes ++ arbitBoxes))

      val request = TransferRequests.ArbitTransferRequest(
        List(sender),
        List(recipient -> 100),
        sender,
        sender,
        0,
        None
      )

      val result = TransferBuilder
        .buildUnsignedArbitTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result.left.value shouldBe BuildTransferFailures.DuplicateInputs
    }
  }

  it should "return invalid if no recipients" in {
    forAll(polyBoxesGen, arbitBoxesGen, addressGen) { (polyBoxes, arbitBoxes, sender) =>
      val boxReader = MockBoxReader.fromNec(sender -> (polyBoxes ++ arbitBoxes))

      val request = TransferRequests.ArbitTransferRequest(
        List(sender),
        List(),
        sender,
        sender,
        0,
        None
      )

      val result = TransferBuilder
        .buildUnsignedArbitTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result.left.value shouldBe BuildTransferFailures.EmptyOutputs
    }
  }

  it should "return invalid if duplicate recipients" in {
    forAll(polyBoxesGen, arbitBoxesGen, addressGen, addressGen) { (polyBoxes, arbitBoxes, sender, recipient) =>
      val boxReader = MockBoxReader.fromNec(sender -> (polyBoxes ++ arbitBoxes))

      val request = TransferRequests.ArbitTransferRequest(
        List(sender),
        List(recipient -> 100, recipient -> 100),
        sender,
        sender,
        0,
        None
      )

      val result = TransferBuilder
        .buildUnsignedArbitTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result.left.value shouldBe BuildTransferFailures.DuplicateOutputs
    }
  }

  it should "return valid if valid inputs" in {
    forAll(polyBoxesGen, arbitBoxesGen, addressGen, addressGen) { (polyBoxes, arbitBoxes, sender, recipient) =>
      val boxReader = MockBoxReader.fromNec(sender -> (polyBoxes ++ arbitBoxes))

      val request = TransferRequests.ArbitTransferRequest(
        List(sender),
        List(recipient -> 100),
        sender,
        sender,
        0,
        None
      )

      val result = TransferBuilder
        .buildUnsignedArbitTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

      result shouldBe Symbol("right")
    }
  }
}
