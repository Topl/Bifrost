package co.topl.modifier.transaction.builder

import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.modifier.box.{ArbitBox, PolyBox}
import co.topl.modifier.{BoxReader, ProgramId}
import co.topl.utils.CommonGenerators
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class BuildUnsignedArbitTransferSpec
    extends AnyFlatSpec
    with CommonGenerators
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with MockFactory
    with EitherValues {

  "buildUnsignedArbitTransfer" should "return invalid if not enough funds for fee" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen, addressGen) {
      (
        firstFeeBox:     PolyBox,
        otherFeeBoxes:   List[PolyBox],
        firstArbitBox:   ArbitBox,
        otherArbitBoxes: List[ArbitBox],
        sender:          Address,
        recipient:       Address
      ) =>
        val feeBoxes = firstFeeBox :: otherFeeBoxes
        val arbitBoxes = firstArbitBox :: otherArbitBoxes
        val fee = feeBoxes.map(_.value.quantity).sum + 100000

        val boxReader = mock[BoxReader[ProgramId, Address]]
        (boxReader.getTokenBoxes _).expects(sender).returns(Some(feeBoxes ++ arbitBoxes))

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

        result shouldBe Symbol("left")
        result.left.value shouldBe BuildTransferFailures.InsufficientFeeFunds
    }
  }

  it should "return invalid if not enough funds for recipients" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen, addressGen) {
      (
        firstFeeBox:     PolyBox,
        otherFeeBoxes:   List[PolyBox],
        firstArbitBox:   ArbitBox,
        otherArbitBoxes: List[ArbitBox],
        sender:          Address,
        recipient:       Address
      ) =>
        val feeBoxes = firstFeeBox :: otherFeeBoxes
        val arbitBoxes = firstArbitBox :: otherArbitBoxes
        val toSend = arbitBoxes.map(_.value.quantity).sum + 10000

        val boxReader = mock[BoxReader[ProgramId, Address]]
        (boxReader.getTokenBoxes _).expects(sender).returns(Some(feeBoxes ++ arbitBoxes))

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

        result shouldBe Symbol("left")
        result.left.value shouldBe BuildTransferFailures.InsufficientPaymentFunds
    }
  }

  it should "return invalid if no input poly boxes" in {
    forAll(arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen, addressGen) {
      (
        firstArbitBox:   ArbitBox,
        otherArbitBoxes: List[ArbitBox],
        sender:          Address,
        recipient:       Address
      ) =>
        val arbitBoxes = firstArbitBox :: otherArbitBoxes

        val boxReader = mock[BoxReader[ProgramId, Address]]
        (boxReader.getTokenBoxes _).expects(sender).returns(Some(arbitBoxes))

        val request = TransferRequests.ArbitTransferRequest(
          List(sender),
          List(recipient -> 1),
          sender,
          sender,
          0,
          None
        )

        val result = TransferBuilder
          .buildUnsignedArbitTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

        result shouldBe Symbol("left")
        result.left.value shouldBe BuildTransferFailures.EmptyInputs
    }
  }

  it should "return invalid if no input arbit boxes" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, addressGen) {
      (
        firstFeeBox:   PolyBox,
        otherFeeBoxes: List[PolyBox],
        sender:        Address,
        recipient:     Address
      ) =>
        val feeBoxes = firstFeeBox :: otherFeeBoxes

        val boxReader = mock[BoxReader[ProgramId, Address]]
        (boxReader.getTokenBoxes _).expects(sender).returns(Some(feeBoxes))

        val request = TransferRequests.ArbitTransferRequest(
          List(sender),
          List(recipient -> 1),
          sender,
          sender,
          0,
          None
        )

        val result = TransferBuilder
          .buildUnsignedArbitTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

        result shouldBe Symbol("left")
        result.left.value shouldBe BuildTransferFailures.EmptyInputs
    }
  }

  it should "return invalid if duplicate poly input boxes" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen, addressGen) {
      (
        firstFeeBox:     PolyBox,
        otherFeeBoxes:   List[PolyBox],
        firstArbitBox:   ArbitBox,
        otherArbitBoxes: List[ArbitBox],
        sender:          Address,
        recipient:       Address
      ) =>
        val feeBoxes = firstFeeBox :: otherFeeBoxes
        val arbitBoxes = firstArbitBox :: otherArbitBoxes

        val boxReader = mock[BoxReader[ProgramId, Address]]
        (boxReader.getTokenBoxes _).expects(sender).returns(Some(feeBoxes ++ feeBoxes ++ arbitBoxes))

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

        result shouldBe Symbol("left")
        result.left.value shouldBe BuildTransferFailures.DuplicateInputs
    }
  }

  it should "return invalid if duplicate arbit input boxes" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen, addressGen) {
      (
        firstFeeBox:     PolyBox,
        otherFeeBoxes:   List[PolyBox],
        firstArbitBox:   ArbitBox,
        otherArbitBoxes: List[ArbitBox],
        sender:          Address,
        recipient:       Address
      ) =>
        val feeBoxes = firstFeeBox :: otherFeeBoxes
        val arbitBoxes = firstArbitBox :: otherArbitBoxes

        val boxReader = mock[BoxReader[ProgramId, Address]]
        (boxReader.getTokenBoxes _).expects(sender).returns(Some(feeBoxes ++ arbitBoxes ++ arbitBoxes))

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

        result shouldBe Symbol("left")
        result.left.value shouldBe BuildTransferFailures.DuplicateInputs
    }
  }

  it should "return invalid if no recipients" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen) {
      (
        firstFeeBox:     PolyBox,
        otherFeeBoxes:   List[PolyBox],
        firstArbitBox:   ArbitBox,
        otherArbitBoxes: List[ArbitBox],
        sender:          Address
      ) =>
        val feeBoxes = firstFeeBox :: otherFeeBoxes
        val arbitBoxes = firstArbitBox :: otherArbitBoxes

        val boxReader = mock[BoxReader[ProgramId, Address]]
        (boxReader.getTokenBoxes _).expects(sender).returns(Some(feeBoxes ++ arbitBoxes))

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

        result shouldBe Symbol("left")
        result.left.value shouldBe BuildTransferFailures.EmptyOutputs
    }
  }

  it should "return invalid if duplicate recipients" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen, addressGen) {
      (
        firstFeeBox:     PolyBox,
        otherFeeBoxes:   List[PolyBox],
        firstArbitBox:   ArbitBox,
        otherArbitBoxes: List[ArbitBox],
        sender:          Address,
        recipient:       Address
      ) =>
        val feeBoxes = firstFeeBox :: otherFeeBoxes
        val arbitBoxes = firstArbitBox :: otherArbitBoxes

        val boxReader = mock[BoxReader[ProgramId, Address]]
        (boxReader.getTokenBoxes _).expects(sender).returns(Some(feeBoxes ++ arbitBoxes))

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

        result shouldBe Symbol("left")
        result.left.value shouldBe BuildTransferFailures.DuplicateOutputs
    }
  }

  it should "return valid if valid inputs" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen, addressGen) {
      (
        firstFeeBox:     PolyBox,
        otherFeeBoxes:   List[PolyBox],
        firstArbitBox:   ArbitBox,
        otherArbitBoxes: List[ArbitBox],
        sender:          Address,
        recipient:       Address
      ) =>
        val feeBoxes = firstFeeBox :: otherFeeBoxes
        val arbitBoxes = firstArbitBox :: otherArbitBoxes

        val boxReader = mock[BoxReader[ProgramId, Address]]
        (boxReader.getTokenBoxes _).expects(sender).returns(Some(feeBoxes ++ arbitBoxes))

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
