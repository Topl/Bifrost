package co.topl.modifier.transaction

import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.modifier.box.{ArbitBox, PolyBox, SimpleValue}
import co.topl.utils.CommonGenerators
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ArbitTransferSpec
    extends AnyFlatSpec
    with CommonGenerators
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with MockFactory
    with EitherValues {

  "ArbitTransferSpec.validated" should "be invalid if not enough funds for fee" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen, addressGen) {
      (
        firstFeeBox:     PolyBox,
        otherFeeBoxes:   List[PolyBox],
        firstArbitBox:   ArbitBox,
        otherArbitBoxes: List[ArbitBox],
        sender:          Address,
        recipient:       Address
      ) =>
        val feeBoxes = firstFeeBox +: otherFeeBoxes
        val arbitBoxes = firstArbitBox +: otherArbitBoxes
        val fee = feeBoxes.map(_.value.quantity).sum + 100000

        val result = ArbitTransfer.validated[PublicKeyPropositionCurve25519](
          feeBoxes.map(sender -> _).toIndexedSeq,
          arbitBoxes.map(sender -> _).toIndexedSeq,
          IndexedSeq(recipient -> SimpleValue(100)),
          sender,
          sender,
          fee,
          None,
          false
        )

        result shouldBe Symbol("left")
        result.left.value shouldBe ArbitTransfer.Validation.InsufficientFunds
    }
  }

  it should "be invalid if not enough funds for recipients" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen, addressGen) {
      (
        firstFeeBox:     PolyBox,
        otherFeeBoxes:   List[PolyBox],
        firstArbitBox:   ArbitBox,
        otherArbitBoxes: List[ArbitBox],
        sender:          Address,
        recipient:       Address
      ) =>
        val feeBoxes = firstFeeBox +: otherFeeBoxes
        val arbitBoxes = firstArbitBox +: otherArbitBoxes

        val result = ArbitTransfer.validated[PublicKeyPropositionCurve25519](
          feeBoxes.map(sender -> _).toIndexedSeq,
          arbitBoxes.map(sender -> _).toIndexedSeq,
          IndexedSeq(recipient -> SimpleValue(arbitBoxes.map(_.value.quantity).sum + 10000)),
          sender,
          sender,
          0,
          None,
          false
        )

        result shouldBe Symbol("left")
        result.left.value shouldBe ArbitTransfer.Validation.InsufficientFunds
    }
  }

  it should "be invalid if no input poly boxes" in {
    forAll(arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen, addressGen) {
      (
        firstArbitBox:   ArbitBox,
        otherArbitBoxes: List[ArbitBox],
        sender:          Address,
        recipient:       Address
      ) =>
        val arbitBoxes = firstArbitBox +: otherArbitBoxes

        val result = ArbitTransfer.validated[PublicKeyPropositionCurve25519](
          IndexedSeq(),
          arbitBoxes.map(sender -> _).toIndexedSeq,
          IndexedSeq(recipient -> SimpleValue(1)),
          sender,
          sender,
          0,
          None,
          false
        )

        result shouldBe Symbol("left")
        result.left.value shouldBe ArbitTransfer.Validation.EmptyPolyInputs
    }
  }

  it should "be invalid if no input arbit boxes" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, addressGen) {
      (
        firstFeeBox:   PolyBox,
        otherFeeBoxes: List[PolyBox],
        sender:        Address,
        recipient:     Address
      ) =>
        val feeBoxes = firstFeeBox +: otherFeeBoxes

        val result = ArbitTransfer.validated[PublicKeyPropositionCurve25519](
          feeBoxes.map(sender -> _).toIndexedSeq,
          IndexedSeq(),
          IndexedSeq(recipient -> SimpleValue(1)),
          sender,
          sender,
          0,
          None,
          false
        )

        result shouldBe Symbol("left")
        result.left.value shouldBe ArbitTransfer.Validation.EmptyArbitInputs
    }
  }

  it should "be invalid if duplicate poly input boxes" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen, addressGen) {
      (
        firstFeeBox:     PolyBox,
        otherFeeBoxes:   List[PolyBox],
        firstArbitBox:   ArbitBox,
        otherArbitBoxes: List[ArbitBox],
        sender:          Address,
        recipient:       Address
      ) =>
        val feeBoxes = firstFeeBox +: otherFeeBoxes
        val arbitBoxes = firstArbitBox +: otherArbitBoxes

        val result = ArbitTransfer.validated[PublicKeyPropositionCurve25519](
          (feeBoxes ++ feeBoxes).map(sender -> _).toIndexedSeq,
          arbitBoxes.map(sender -> _).toIndexedSeq,
          IndexedSeq(recipient -> SimpleValue(100)),
          sender,
          sender,
          0,
          None,
          false
        )

        result shouldBe Symbol("left")
        result.left.value shouldBe ArbitTransfer.Validation.DuplicatePolyInputs
    }
  }

  it should "be invalid if duplicate arbit input boxes" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen, addressGen) {
      (
        firstFeeBox:     PolyBox,
        otherFeeBoxes:   List[PolyBox],
        firstArbitBox:   ArbitBox,
        otherArbitBoxes: List[ArbitBox],
        sender:          Address,
        recipient:       Address
      ) =>
        val feeBoxes = firstFeeBox +: otherFeeBoxes
        val arbitBoxes = firstArbitBox +: otherArbitBoxes

        val result = ArbitTransfer.validated[PublicKeyPropositionCurve25519](
          feeBoxes.map(sender -> _).toIndexedSeq,
          (arbitBoxes ++ arbitBoxes).map(sender -> _).toIndexedSeq,
          IndexedSeq(recipient -> SimpleValue(100)),
          sender,
          sender,
          0,
          None,
          false
        )

        result shouldBe Symbol("left")
        result.left.value shouldBe ArbitTransfer.Validation.DuplicateArbitInputs
    }
  }

  it should "be invalid if no recipients" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen) {
      (
        firstFeeBox:     PolyBox,
        otherFeeBoxes:   List[PolyBox],
        firstArbitBox:   ArbitBox,
        otherArbitBoxes: List[ArbitBox],
        sender:          Address
      ) =>
        val feeBoxes = firstFeeBox +: otherFeeBoxes
        val arbitBoxes = firstArbitBox +: otherArbitBoxes

        val result = ArbitTransfer.validated[PublicKeyPropositionCurve25519](
          feeBoxes.map(sender -> _).toIndexedSeq,
          arbitBoxes.map(sender -> _).toIndexedSeq,
          IndexedSeq(),
          sender,
          sender,
          0,
          None,
          false
        )

        result shouldBe Symbol("left")
        result.left.value shouldBe ArbitTransfer.Validation.NoRecipients
    }
  }

  it should "be invalid if duplicate recipients" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen, addressGen) {
      (
        firstFeeBox:     PolyBox,
        otherFeeBoxes:   List[PolyBox],
        firstArbitBox:   ArbitBox,
        otherArbitBoxes: List[ArbitBox],
        sender:          Address,
        recipient:       Address
      ) =>
        val feeBoxes = firstFeeBox +: otherFeeBoxes
        val arbitBoxes = firstArbitBox +: otherArbitBoxes

        val result = ArbitTransfer.validated[PublicKeyPropositionCurve25519](
          feeBoxes.map(sender -> _).toIndexedSeq,
          arbitBoxes.map(sender -> _).toIndexedSeq,
          IndexedSeq(recipient -> SimpleValue(100), recipient -> SimpleValue(100)),
          sender,
          sender,
          0,
          None,
          false
        )

        result shouldBe Symbol("left")
        result.left.value shouldBe ArbitTransfer.Validation.NonUniqueRecipients
    }
  }

  it should "be valid if valid inputs" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen, addressGen) {
      (
        firstFeeBox:     PolyBox,
        otherFeeBoxes:   List[PolyBox],
        firstArbitBox:   ArbitBox,
        otherArbitBoxes: List[ArbitBox],
        sender:          Address,
        recipient:       Address
      ) =>
        val feeBoxes = firstFeeBox +: otherFeeBoxes
        val arbitBoxes = firstArbitBox +: otherArbitBoxes

        val result = ArbitTransfer.validated[PublicKeyPropositionCurve25519](
          feeBoxes.map(sender -> _).toIndexedSeq,
          arbitBoxes.map(sender -> _).toIndexedSeq,
          IndexedSeq(recipient -> SimpleValue(1)),
          sender,
          sender,
          0,
          None,
          false
        )

        result shouldBe Symbol("right")
    }
  }
}
