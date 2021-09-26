//package co.topl.modifier.transaction.builder
//
//import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
//import co.topl.modifier.box.{PolyBox, SimpleValue}
//import co.topl.modifier.transaction.PolyTransfer
//import co.topl.utils.{CommonGenerators, Int128}
//import org.scalacheck.Gen
//import org.scalamock.scalatest.MockFactory
//import org.scalatest.EitherValues
//import org.scalatest.flatspec.AnyFlatSpec
//import org.scalatest.matchers.should.Matchers
//import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
//
//class PolyTransferRequestSpec
//    extends AnyFlatSpec
//    with CommonGenerators
//    with Matchers
//    with ScalaCheckDrivenPropertyChecks
//    with MockFactory
//    with EitherValues {
//
//  "unsignedTxFromPolyTransferRequest" should "be invalid if not enough funds for fee" in {
//    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, addressGen) {
//      (firstBox: PolyBox, otherBoxes: List[PolyBox], sender: Address, recipient: Address) =>
//        val fromBoxes = firstBox +: otherBoxes
//        val fee = fromBoxes.map(_.value.quantity).sum + 100000
//
//        val request = TransferRequests.PolyTransferRequest(
//          List(sender),
//          List(recipient -> 100),
//          sender,
//          fee,
//          None
//        )
//
//        val result = fromPolyTransferRequest[PublicKeyPropositionCurve25519](request, fromBoxes.map(sender -> _))
//
//        result shouldBe Symbol("left")
//        result.left.value shouldBe UnsignedTransferFailures.InsufficientFeeFunds
//    }
//  }
//
//  it should "be invalid if not enough funds for recipients" in {
//    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, addressGen) {
//      (firstBox: PolyBox, otherBoxes: List[PolyBox], sender: Address, recipient: Address) =>
//        val fromBoxes = (firstBox +: otherBoxes).map(sender -> _)
//        val paymentAmount: Int128 = fromBoxes.map(_._2.value.quantity).sum + 100
//
//        val request = TransferRequests.PolyTransferRequest(
//          List(sender),
//          List(recipient -> paymentAmount),
//          sender,
//          0,
//          None
//        )
//
//        val result = fromPolyTransferRequest[PublicKeyPropositionCurve25519](request, fromBoxes)
//
//        result shouldBe Symbol("left")
//        result.left.value shouldBe UnsignedTransferFailures.InsufficientPaymentFunds
//    }
//  }
//
//  it should "be invalid if no input boxes" in {
//    forAll(addressGen, addressGen) { (sender: Address, recipient: Address) =>
//      val request = TransferRequests.PolyTransferRequest(
//        List(sender),
//        List(recipient -> 100),
//        sender,
//        0,
//        None
//      )
//
//      val result = fromPolyTransferRequest[PublicKeyPropositionCurve25519](request, List())
//
//      result shouldBe Symbol("left")
//      result.left.value shouldBe UnsignedTransferFailures.EmptyInputs
//    }
//  }
//
//  it should "be invalid if duplicate input boxes" in {
//    forAll(polyBoxGen, addressGen, addressGen) { (box: PolyBox, sender: Address, recipient: Address) =>
//      val request = TransferRequests.PolyTransferRequest(
//        List(sender),
//        List(recipient -> 100),
//        sender,
//        0,
//        None
//      )
//
//      val result = fromPolyTransferRequest[PublicKeyPropositionCurve25519](request, List(sender -> box, sender -> box))
//
//      result shouldBe Symbol("left")
//      result.left.value shouldBe UnsignedTransferFailures.DuplicateInputs
//    }
//  }
//
//  it should "be invalid if no recipients" in {
//    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen) {
//      (firstBox: PolyBox, otherBoxes: List[PolyBox], sender: Address) =>
//        val fromBoxes = (firstBox +: otherBoxes).map(sender -> _)
//
//        val request = TransferRequests.PolyTransferRequest(
//          List(sender),
//          List(),
//          sender,
//          0,
//          None
//        )
//
//        val result = fromPolyTransferRequest[PublicKeyPropositionCurve25519](request, fromBoxes)
//
//        result shouldBe Symbol("left")
//        result.left.value shouldBe UnsignedTransferFailures.EmptyRecipients
//    }
//  }
//
//  it should "be invalid if duplicate recipients" in {
//    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, addressGen) {
//      (firstBox: PolyBox, otherBoxes: List[PolyBox], sender: Address, recipient: Address) =>
//        val fromBoxes = (firstBox +: otherBoxes).map(sender -> _)
//
//        val request = TransferRequests.PolyTransferRequest(
//          List(sender),
//          List(recipient -> 0, recipient -> 0),
//          sender,
//          0,
//          None
//        )
//
//        val result = fromPolyTransferRequest[PublicKeyPropositionCurve25519](request, fromBoxes)
//
//        result shouldBe Symbol("left")
//        result.left.value shouldBe UnsignedTransferFailures.DuplicateRecipients
//    }
//  }
//
//  it should "be valid if valid inputs" in {
//    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, addressGen) {
//      (firstBox: PolyBox, otherBoxes: List[PolyBox], sender: Address, recipient: Address) =>
//        val request = TransferRequests.PolyTransferRequest(
//          List(sender),
//          List(recipient -> 1),
//          sender,
//          1,
//          None
//        )
//
//        val result =
//          fromPolyTransferRequest[PublicKeyPropositionCurve25519](request, (firstBox +: otherBoxes).map(sender -> _))
//
//        result shouldBe Symbol("right")
//    }
//  }
//}
