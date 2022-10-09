package co.topl.modifier.transaction.builder

import co.topl.attestation.{Address, Evidence, PublicKeyPropositionCurve25519}
import co.topl.modifier.box.{ArbitBox, PolyBox, SimpleValue}
import co.topl.modifier.transaction.PolyTransfer
import co.topl.modifier.transaction.builder.Generators._
import co.topl.utils.encode.Base16
import co.topl.utils.{CommonGenerators, Int128}
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.Instant
import scala.collection.immutable.ListMap

class BuildUnsignedPolyTransferSpec
    extends AnyFlatSpec
    with CommonGenerators
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with MockFactory
    with EitherValues {

  behavior of "buildUnsignedPolyTransfer"

  it should "construct an expected transaction given a fixed input" in {
    val addressB = Address(
      Evidence(Base16.decode("01b2f6eea5ede8c91af2d7228c29bd0586e0502107cba19a8df9c9f21ddee3d529").toOption.get)
    )
    val addressC = Address(
      Evidence(Base16.decode("011c45ee2f221526357aa59e943ae83ed6db0809413426e75c325de39b849a36ba").toOption.get)
    )

    val polyBoxes = Seq(
      PolyBox(
        addressB.evidence,
        2630159458189048846L,
        SimpleValue(BigInt(40))
      ),
      PolyBox(
        addressB.evidence,
        3107467157757088833L,
        SimpleValue(BigInt(40))
      ),
      PolyBox(
        addressB.evidence,
        -4026852029385248121L,
        SimpleValue(BigInt(10))
      ),
      PolyBox(
        addressB.evidence,
        697302988258087177L,
        SimpleValue(BigInt(20))
      )
    )

    val fee = 0

    val polysToSend = 10

    val boxReader = MockBoxReader.fromSeq(addressB -> polyBoxes)

    val request = TransferRequests.PolyTransferRequest(
      List(addressB),
      List(addressC -> polysToSend),
      addressB,
      fee,
      None
    )

    val result = TransferBuilder
      .buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519](boxReader, request, BoxSelectionAlgorithms.All)

    result.map(println)

    result.isRight shouldBe true

  }

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
