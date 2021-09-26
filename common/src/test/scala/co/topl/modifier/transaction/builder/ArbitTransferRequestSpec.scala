//package co.topl.modifier.transaction.builder
//
//import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
//import co.topl.modifier.box.{ArbitBox, PolyBox, SimpleValue}
//import co.topl.modifier.transaction.ArbitTransfer
//import co.topl.modifier.transaction.builder.{Builder, TransferRequests}
//import co.topl.utils.CommonGenerators
//import org.scalacheck.Gen
//import org.scalamock.scalatest.MockFactory
//import org.scalatest.EitherValues
//import org.scalatest.flatspec.AnyFlatSpec
//import org.scalatest.matchers.should.Matchers
//import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
//
//class ArbitTransferRequestSpec
//    extends AnyFlatSpec
//    with CommonGenerators
//    with Matchers
//    with ScalaCheckDrivenPropertyChecks
//    with MockFactory
//    with EitherValues {
//
//  "unsignedTxFromArbitTransferRequest" should "be invalid if not enough funds for fee" in {
//    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen, addressGen) {
//      (
//        firstFeeBox:     PolyBox,
//        otherFeeBoxes:   List[PolyBox],
//        firstArbitBox:   ArbitBox,
//        otherArbitBoxes: List[ArbitBox],
//        sender:          Address,
//        recipient:       Address
//      ) =>
//        val feeBoxes = otherFeeBoxes.prepended(firstFeeBox).map(sender -> _)
//        val arbitBoxes = otherArbitBoxes.prepended(firstArbitBox).map(sender -> _)
//        val fee = feeBoxes.map(_._2.value.quantity).sum + 100000
//
//        val request = TransferRequests.ArbitTransferRequest(
//          List(sender),
//          List(recipient -> 100),
//          sender,
//          sender,
//          fee,
//          None
//        )
//
//        val result = Builder.buildUnsignedArbitTransfer[PublicKeyPropositionCurve25519](request, feeBoxes, arbitBoxes)
//
//        result shouldBe Symbol("left")
//        result.left.value shouldBe UnsignedTransferFailures.InsufficientFeeFunds
//    }
//  }
//
//  it should "be invalid if not enough funds for recipients" in {
//    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen, addressGen) {
//      (
//        firstFeeBox:     PolyBox,
//        otherFeeBoxes:   List[PolyBox],
//        firstArbitBox:   ArbitBox,
//        otherArbitBoxes: List[ArbitBox],
//        sender:          Address,
//        recipient:       Address
//      ) =>
//        val feeBoxes = otherFeeBoxes.prepended(firstFeeBox).map(sender -> _)
//        val arbitBoxes = otherArbitBoxes.prepended(firstArbitBox).map(sender -> _)
//
//        val toSend = arbitBoxes.map(_._2.value.quantity).sum + 10000
//
//        val request = TransferRequests.ArbitTransferRequest(
//          List(sender),
//          List(recipient -> toSend),
//          sender,
//          sender,
//          0,
//          None
//        )
//
//        val result = fromArbitTransferRequest[PublicKeyPropositionCurve25519](request, feeBoxes, arbitBoxes)
//
//        result shouldBe Symbol("left")
//        result.left.value shouldBe UnsignedTransferFailures.InsufficientPaymentFunds
//    }
//  }
//
//  it should "be invalid if no input poly boxes" in {
//    forAll(arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen, addressGen) {
//      (
//        firstArbitBox:   ArbitBox,
//        otherArbitBoxes: List[ArbitBox],
//        sender:          Address,
//        recipient:       Address
//      ) =>
//        val arbitBoxes = otherArbitBoxes.prepended(firstArbitBox).map(sender -> _)
//
//        val request = TransferRequests.ArbitTransferRequest(
//          List(sender),
//          List(recipient -> 1),
//          sender,
//          sender,
//          0,
//          None
//        )
//
//        val result = fromArbitTransferRequest[PublicKeyPropositionCurve25519](request, List(), arbitBoxes)
//
//        result shouldBe Symbol("left")
//        result.left.value shouldBe UnsignedTransferFailures.EmptyInputs
//    }
//  }
//
//  it should "be invalid if no input arbit boxes" in {
//    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, addressGen) {
//      (
//        firstFeeBox:   PolyBox,
//        otherFeeBoxes: List[PolyBox],
//        sender:        Address,
//        recipient:     Address
//      ) =>
//        val feeBoxes = otherFeeBoxes.prepended(firstFeeBox).map(sender -> _)
//
//        val request = TransferRequests.ArbitTransferRequest(
//          List(sender),
//          List(recipient -> 1),
//          sender,
//          sender,
//          0,
//          None
//        )
//
//        val result = fromArbitTransferRequest[PublicKeyPropositionCurve25519](request, feeBoxes, List())
//
//        result shouldBe Symbol("left")
//        result.left.value shouldBe UnsignedTransferFailures.EmptyInputs
//    }
//  }
//
//  it should "be invalid if duplicate poly input boxes" in {
//    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen, addressGen) {
//      (
//        firstFeeBox:     PolyBox,
//        otherFeeBoxes:   List[PolyBox],
//        firstArbitBox:   ArbitBox,
//        otherArbitBoxes: List[ArbitBox],
//        sender:          Address,
//        recipient:       Address
//      ) =>
//        val feeBoxes = otherFeeBoxes.prepended(firstFeeBox).map(sender -> _)
//        val arbitBoxes = otherArbitBoxes.prepended(firstArbitBox).map(sender -> _)
//
//        val request = TransferRequests.ArbitTransferRequest(
//          List(sender),
//          List(recipient -> 100),
//          sender,
//          sender,
//          0,
//          None
//        )
//
//        val result = fromArbitTransferRequest[PublicKeyPropositionCurve25519](request, feeBoxes ++ feeBoxes, arbitBoxes)
//
//        result shouldBe Symbol("left")
//        result.left.value shouldBe UnsignedTransferFailures.DuplicateInputs
//    }
//  }
//
//  it should "be invalid if duplicate arbit input boxes" in {
//    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen, addressGen) {
//      (
//        firstFeeBox:     PolyBox,
//        otherFeeBoxes:   List[PolyBox],
//        firstArbitBox:   ArbitBox,
//        otherArbitBoxes: List[ArbitBox],
//        sender:          Address,
//        recipient:       Address
//      ) =>
//        val feeBoxes = otherFeeBoxes.prepended(firstFeeBox).map(sender -> _)
//        val arbitBoxes = otherArbitBoxes.prepended(firstArbitBox).map(sender -> _)
//
//        val request = TransferRequests.ArbitTransferRequest(
//          List(sender),
//          List(recipient -> 100),
//          sender,
//          sender,
//          0,
//          None
//        )
//
//        val result =
//          fromArbitTransferRequest[PublicKeyPropositionCurve25519](request, feeBoxes, arbitBoxes ++ arbitBoxes)
//
//        result shouldBe Symbol("left")
//        result.left.value shouldBe UnsignedTransferFailures.DuplicateInputs
//    }
//  }
//
//  it should "be invalid if no recipients" in {
//    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen) {
//      (
//        firstFeeBox:     PolyBox,
//        otherFeeBoxes:   List[PolyBox],
//        firstArbitBox:   ArbitBox,
//        otherArbitBoxes: List[ArbitBox],
//        sender:          Address
//      ) =>
//        val feeBoxes = otherFeeBoxes.prepended(firstFeeBox).map(sender -> _)
//        val arbitBoxes = otherArbitBoxes.prepended(firstArbitBox).map(sender -> _)
//
//        val request = TransferRequests.ArbitTransferRequest(
//          List(sender),
//          List(),
//          sender,
//          sender,
//          0,
//          None
//        )
//
//        val result = fromArbitTransferRequest[PublicKeyPropositionCurve25519](request, feeBoxes, arbitBoxes)
//
//        result shouldBe Symbol("left")
//        result.left.value shouldBe UnsignedTransferFailures.EmptyRecipients
//    }
//  }
//
//  it should "be invalid if duplicate recipients" in {
//    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen, addressGen) {
//      (
//        firstFeeBox:     PolyBox,
//        otherFeeBoxes:   List[PolyBox],
//        firstArbitBox:   ArbitBox,
//        otherArbitBoxes: List[ArbitBox],
//        sender:          Address,
//        recipient:       Address
//      ) =>
//        val feeBoxes = otherFeeBoxes.prepended(firstFeeBox).map(sender -> _)
//        val arbitBoxes = otherArbitBoxes.prepended(firstArbitBox).map(sender -> _)
//
//        val request = TransferRequests.ArbitTransferRequest(
//          List(sender),
//          List(recipient -> 100, recipient -> 100),
//          sender,
//          sender,
//          0,
//          None
//        )
//
//        val result = fromArbitTransferRequest[PublicKeyPropositionCurve25519](request, feeBoxes, arbitBoxes)
//
//        result shouldBe Symbol("left")
//        result.left.value shouldBe UnsignedTransferFailures.DuplicateRecipients
//    }
//  }
//
//  it should "be valid if valid inputs" in {
//    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen, addressGen) {
//      (
//        firstFeeBox:     PolyBox,
//        otherFeeBoxes:   List[PolyBox],
//        firstArbitBox:   ArbitBox,
//        otherArbitBoxes: List[ArbitBox],
//        sender:          Address,
//        recipient:       Address
//      ) =>
//        val feeBoxes = otherFeeBoxes.prepended(firstFeeBox).map(sender -> _)
//        val arbitBoxes = otherArbitBoxes.prepended(firstArbitBox).map(sender -> _)
//
//        val request = TransferRequests.ArbitTransferRequest(
//          List(sender),
//          List(recipient -> 100),
//          sender,
//          sender,
//          0,
//          None
//        )
//
//        val result = fromArbitTransferRequest[PublicKeyPropositionCurve25519](request, feeBoxes, arbitBoxes)
//
//        result shouldBe Symbol("right")
//    }
//  }
//}
