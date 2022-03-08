package co.topl.modifier.transaction.builder

import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.modifier.box.PolyBox
import co.topl.modifier.{BoxReader, ProgramId}
import co.topl.utils.{CommonGenerators, Int128}
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class BuildUnsignedPolyTransferSpec
    extends AnyFlatSpec
    with CommonGenerators
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with MockFactory
    with EitherValues {

  "buildUnsignedPolyTransfer" should "return invalid if not enough funds for fee" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, addressGen) {
      (firstBox: PolyBox, otherBoxes: List[PolyBox], sender: Address, recipient: Address) =>
        val fromBoxes = firstBox :: otherBoxes
        val fee = fromBoxes.map(_.value.quantity).sum + 100000

        val boxReader = mock[BoxReader[ProgramId, Address]]
        (boxReader.getTokenBoxes _).expects(sender).returns(Some(fromBoxes))

        val request = TransferRequests.PolyTransferRequest(
          List(sender),
          List(recipient -> 100),
          sender,
          fee,
          None
        )

        val result = TransferBuilder
          .buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

        result shouldBe Symbol("left")
        result.left.value shouldBe BuildTransferFailures.InsufficientFeeFunds
    }
  }

  it should "return invalid if not enough funds for recipients" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, addressGen) {
      (firstBox: PolyBox, otherBoxes: List[PolyBox], sender: Address, recipient: Address) =>
        val fromBoxes = firstBox :: otherBoxes
        val paymentAmount: Int128 = fromBoxes.map(_.value.quantity).sum + 100

        val boxReader = mock[BoxReader[ProgramId, Address]]
        (boxReader.getTokenBoxes _).expects(sender).returns(Some(fromBoxes))

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

        result shouldBe Symbol("left")
        result.left.value shouldBe BuildTransferFailures.InsufficientPaymentFunds
    }
  }

  it should "return invalid if no input boxes" in {
    forAll(addressGen, addressGen) { (sender: Address, recipient: Address) =>
      val boxReader = mock[BoxReader[ProgramId, Address]]
      (boxReader.getTokenBoxes _).expects(sender).returns(Some(List()))

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

      result shouldBe Symbol("left")
      result.left.value shouldBe BuildTransferFailures.EmptyInputs
    }
  }

  it should "return invalid if duplicate input boxes" in {
    forAll(polyBoxGen, addressGen, addressGen) { (box: PolyBox, sender: Address, recipient: Address) =>
      val boxReader = mock[BoxReader[ProgramId, Address]]
      (boxReader.getTokenBoxes _).expects(sender).returns(Some(List(box, box)))

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

      result shouldBe Symbol("left")
      result.left.value shouldBe BuildTransferFailures.DuplicateInputs
    }
  }

  it should "return invalid if no recipients" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen) {
      (firstBox: PolyBox, otherBoxes: List[PolyBox], sender: Address) =>
        val fromBoxes = firstBox :: otherBoxes

        val boxReader = mock[BoxReader[ProgramId, Address]]
        (boxReader.getTokenBoxes _).expects(sender).returns(Some(fromBoxes))

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

        result shouldBe Symbol("left")
        result.left.value shouldBe BuildTransferFailures.EmptyOutputs
    }
  }

  it should "return invalid if duplicate recipients" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, addressGen) {
      (firstBox: PolyBox, otherBoxes: List[PolyBox], sender: Address, recipient: Address) =>
        val fromBoxes = firstBox :: otherBoxes

        val boxReader = mock[BoxReader[ProgramId, Address]]
        (boxReader.getTokenBoxes _).expects(sender).returns(Some(fromBoxes))

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

        result shouldBe Symbol("left")
        result.left.value shouldBe BuildTransferFailures.DuplicateOutputs
    }
  }

  it should "return valid if valid inputs" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, addressGen) {
      (firstBox: PolyBox, otherBoxes: List[PolyBox], sender: Address, recipient: Address) =>
        val boxReader = mock[BoxReader[ProgramId, Address]]
        (boxReader.getTokenBoxes _).expects(sender).returns(Some(firstBox +: otherBoxes))

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
