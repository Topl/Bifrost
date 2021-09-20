package co.topl.modifier.transaction

import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.modifier.box.{PolyBox, SimpleValue}
import co.topl.utils.CommonGenerators
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class PolyTransferSpec
    extends AnyFlatSpec
    with CommonGenerators
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with MockFactory
    with EitherValues {

  "PolyTransfer.validated" should "be invalid if not enough funds for fee" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, addressGen) {
      (firstBox: PolyBox, otherBoxes: List[PolyBox], sender: Address, recipient: Address) =>
        val fromBoxes = firstBox +: otherBoxes
        val fee = fromBoxes.map(_.value.quantity).sum + 100000

        val result = PolyTransfer.validated[PublicKeyPropositionCurve25519](
          fromBoxes.map(sender -> _).toIndexedSeq,
          IndexedSeq(recipient -> SimpleValue(100)),
          sender,
          fee,
          None,
          false
        )

        result shouldBe Symbol("left")
        result.left.value shouldBe PolyTransfer.Validation.InsufficientFunds
    }
  }

  it should "be invalid if not enough funds for recipients" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, addressGen) {
      (firstBox: PolyBox, otherBoxes: List[PolyBox], sender: Address, recipient: Address) =>
        val fromBoxes = firstBox +: otherBoxes
        val paymentAmount = fromBoxes.map(_.value.quantity).sum

        val result = PolyTransfer.validated[PublicKeyPropositionCurve25519](
          fromBoxes.map(sender -> _).toIndexedSeq,
          IndexedSeq(recipient -> SimpleValue(paymentAmount + 100)),
          sender,
          0,
          None,
          false
        )

        result shouldBe Symbol("left")
        result.left.value shouldBe PolyTransfer.Validation.InsufficientFunds
    }
  }

  it should "be invalid if no input boxes" in {
    forAll(addressGen, addressGen) { (sender: Address, recipient: Address) =>
      val result = PolyTransfer.validated[PublicKeyPropositionCurve25519](
        IndexedSeq(),
        IndexedSeq(recipient -> SimpleValue(100)),
        sender,
        0,
        None,
        false
      )

      result shouldBe Symbol("left")
      result.left.value shouldBe PolyTransfer.Validation.NoInputBoxes
    }
  }

  it should "be invalid if duplicate input boxes" in {
    forAll(polyBoxGen, addressGen, addressGen) { (box: PolyBox, sender: Address, recipient: Address) =>
      val result = PolyTransfer.validated[PublicKeyPropositionCurve25519](
        IndexedSeq(sender    -> box, sender -> box),
        IndexedSeq(recipient -> SimpleValue(100)),
        sender,
        0,
        None,
        false
      )

      result shouldBe Symbol("left")
      result.left.value shouldBe PolyTransfer.Validation.NonUniqueInputs
    }
  }

  it should "be invalid if no recipients" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen) {
      (firstBox: PolyBox, otherBoxes: List[PolyBox], sender: Address) =>
        val fromBoxes = firstBox +: otherBoxes

        val result = PolyTransfer.validated[PublicKeyPropositionCurve25519](
          fromBoxes.map(sender -> _).toIndexedSeq,
          IndexedSeq(),
          sender,
          0,
          None,
          false
        )

        result shouldBe Symbol("left")
        result.left.value shouldBe PolyTransfer.Validation.NoRecipients
    }
  }

  it should "be invalid if duplicate recipients" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, addressGen) {
      (firstBox: PolyBox, otherBoxes: List[PolyBox], sender: Address, recipient: Address) =>
        val fromBoxes = firstBox +: otherBoxes

        val result = PolyTransfer.validated[PublicKeyPropositionCurve25519](
          fromBoxes.map(sender -> _).toIndexedSeq,
          IndexedSeq(recipient -> SimpleValue(0), recipient -> SimpleValue(0)),
          sender,
          0,
          None,
          false
        )

        result shouldBe Symbol("left")
        result.left.value shouldBe PolyTransfer.Validation.NonUniqueRecipients
    }
  }

  it should "be valid if valid inputs" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, addressGen) {
      (firstBox: PolyBox, otherBoxes: List[PolyBox], sender: Address, recipient: Address) =>
        val result = PolyTransfer.validated[PublicKeyPropositionCurve25519](
          (firstBox +: otherBoxes).map(sender -> _).toIndexedSeq,
          IndexedSeq(recipient -> SimpleValue(1)),
          sender,
          1,
          None,
          false
        )

        result shouldBe Symbol("right")
    }
  }
}
