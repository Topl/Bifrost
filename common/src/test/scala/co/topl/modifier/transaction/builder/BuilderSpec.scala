package co.topl.modifier.transaction.builder

import cats.implicits._
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.modifier.BoxReader
import co.topl.modifier.box.{PolyBox, ProgramId, SimpleValue}
import co.topl.modifier.transaction.PolyTransfer.Validation.InvalidPolyTransfer
import co.topl.modifier.transaction.builder.implicits._
import co.topl.modifier.transaction.{builder, PolyTransfer}
import co.topl.utils.CommonGenerators
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Random

class TransferBuilderSpec
    extends AnyFlatSpec
    with CommonGenerators
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with MockFactory
    with EitherValues {

  val random = new Random()

  "buildTransfer" should "use all boxes when using 'All' strategy" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, addressGen) {
      (firstBox: PolyBox, otherBoxes: List[PolyBox], sender: Address, recipient: Address) =>
        val boxReader = mock[BoxReader[ProgramId, Address]]

        val polyBoxes = random.shuffle(firstBox +: otherBoxes)

        (boxReader.getTokenBoxes _).expects(sender).returns(Some(polyBoxes))

        val buildResult: Either[InvalidPolyTransfer, PolyTransfer[PublicKeyPropositionCurve25519]] =
          builder.buildTransfer[SimpleValue, InvalidPolyTransfer, PolyTransfer[
            PublicKeyPropositionCurve25519
          ], BoxPickingStrategy.All](
            senders = IndexedSeq(sender),
            recipients = IndexedSeq(recipient -> SimpleValue(polyBoxes.map(_.value.quantity).sum)),
            boxReader = boxReader,
            feeChangeAddress = sender,
            consolidationAddress = sender,
            fee = 0,
            data = None,
            minting = false,
            strategy = BoxPickingStrategy.All
          )

        // check that the same nonces are in the result as in the inputs
        buildResult.value.from.map(_._2).sorted shouldBe polyBoxes.map(_.nonce).sorted
    }
  }

  it should "use the specified boxes when using 'Specific' strategy" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, addressGen) {
      (specificBox: PolyBox, otherBoxes: List[PolyBox], sender: Address, recipient: Address) =>
        val boxReader = mock[BoxReader[ProgramId, Address]]

        val polyBoxes = random.shuffle(specificBox +: otherBoxes)

        (boxReader.getTokenBoxes _).expects(sender).returns(Some(polyBoxes))

        val buildResult: Either[InvalidPolyTransfer, PolyTransfer[PublicKeyPropositionCurve25519]] =
          builder.buildTransfer[SimpleValue, InvalidPolyTransfer, PolyTransfer[
            PublicKeyPropositionCurve25519
          ], BoxPickingStrategy.Specific](
            senders = IndexedSeq(sender),
            recipients = IndexedSeq(recipient -> SimpleValue(specificBox.value.quantity)),
            boxReader = boxReader,
            feeChangeAddress = sender,
            consolidationAddress = sender,
            fee = 0,
            data = None,
            minting = false,
            strategy = BoxPickingStrategy.Specific(IndexedSeq(specificBox.nonce))
          )

        // check that the nonces are the same
        buildResult.value.from.map(_._2).sorted shouldBe List(specificBox).map(_.nonce).sorted
    }
  }

  it should "use the smallest boxes first when using 'SmallestFirst' strategy" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen, addressGen) {
      (firstBox: PolyBox, otherBoxes: List[PolyBox], sender: Address, recipient: Address) =>
        val boxReader = mock[BoxReader[ProgramId, Address]]

        val polyBoxes = random.shuffle(firstBox +: otherBoxes)

        val smallestBox = polyBoxes.sorted(Ordering.by((x: PolyBox) => x.value.quantity)).head

        (boxReader.getTokenBoxes _).expects(sender).returns(Some(polyBoxes))

        val buildResult =
          builder.buildTransfer[SimpleValue, InvalidPolyTransfer, PolyTransfer[
            PublicKeyPropositionCurve25519
          ], BoxPickingStrategy.SmallestFirst](
            senders = IndexedSeq(sender),
            recipients = IndexedSeq(recipient -> smallestBox.value),
            boxReader = boxReader,
            feeChangeAddress = sender,
            consolidationAddress = sender,
            fee = 0,
            data = None,
            minting = false,
            strategy = BoxPickingStrategy.SmallestFirst(None, smallestBox.value.quantity.some, None)
          )

        // check that the only box used is the smallest one
        buildResult.value.from.map(_._2).sorted shouldBe List(smallestBox).map(_.nonce).sorted
    }
  }
}
