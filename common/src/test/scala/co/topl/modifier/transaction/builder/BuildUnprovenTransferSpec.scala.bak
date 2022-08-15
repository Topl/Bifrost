package co.topl.modifier.transaction.builder

import cats.data.NonEmptyChain
import cats.implicits._
import co.topl.attestation.implicits._
import co.topl.models.utility.HasLength.instances.bigIntLength
import co.topl.models.utility.{Lengths, Sized}
import co.topl.models.{Box => TetraBox, Int128, ModelGenerators, Transaction}
import co.topl.modifier.box._
import co.topl.modifier.ops.implicits._
import co.topl.modifier.transaction.builder.Generators._
import co.topl.utils.CommonGenerators
import co.topl.utils.ops.implicits._
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{EitherValues, OptionValues}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scodec.bits.ByteVector

class BuildUnprovenTransferSpec
    extends AnyFlatSpec
    with CommonGenerators
    with Matchers
    with EitherValues
    with OptionValues
    with ScalaCheckDrivenPropertyChecks {
  behavior of "TransferBuilder.buildUnprovenTransfer"

  val zeroInt128: Int128 = Sized.maxUnsafe[BigInt, Lengths.`128`.type](0)

  it should "use all available poly boxes when sending polys" in {
    forAll(
      dionAddressesGen,
      ModelGenerators.arbitraryPositiveInt128.arbitrary,
      ModelGenerators.arbitraryPolyOutputs.arbitrary,
      polyBoxGen
    ) { (fromAddresses, fee, polyOutputs, polyBox) =>
      val polysSent = polyOutputsAmount(polyOutputs.toList)

      val boxReader =
        MockBoxReader.fromSeq(
          fromAddresses.head.toAddress -> Seq(
            polyBox.copy(value = SimpleValue(polysSent + fee.data))
          )
        )

      val request =
        TransferRequests.UnprovenTransferRequest(
          fromAddresses.toList,
          polyOutputs.toList,
          fromAddresses.head,
          fromAddresses.head,
          fee,
          None,
          minting = false
        )

      val transferResult = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      transferResult.value.inputs.map(_._2) should contain(polyBox.nonce)
    }
  }

  it should "not use any available arbit boxes when only polys are sent" in {
    forAll(
      dionAddressesGen,
      dionAddressesGen,
      ModelGenerators.arbitraryPositiveInt128.arbitrary,
      ModelGenerators.arbitraryPositiveInt128.arbitrary,
      polyBoxGen,
      arbitBoxGen
    ) { (fromAddresses, toAddresses, fee, sendAmount, polyBox, arbitBox) =>
      val polyOutputs = toAddresses.map(address => Transaction.PolyOutput(address, sendAmount))

      val boxReader =
        MockBoxReader.fromSeq(
          fromAddresses.head.toAddress ->
          Seq(polyBox.copy(value = SimpleValue(polyOutputsAmount(polyOutputs.toList) + fee.data)), arbitBox)
        )

      val request =
        TransferRequests.UnprovenTransferRequest(
          fromAddresses.toList,
          polyOutputs.toList,
          fromAddresses.head,
          fromAddresses.head,
          fee,
          None,
          minting = false
        )

      val transferResult = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      transferResult.value.inputs.map(_._2) should not contain arbitBox.nonce
    }
  }

  it should "not use any available asset boxes when only polys are sent" in {
    forAll(
      dionAddressesGen,
      dionAddressesGen,
      ModelGenerators.arbitraryPositiveInt128.arbitrary,
      ModelGenerators.arbitraryPositiveInt128.arbitrary,
      polyBoxGen,
      assetBoxGen
    ) { (fromAddresses, toAddresses, fee, sendAmount, polyBox, assetBox) =>
      val polyOutputs = toAddresses.map(address => Transaction.PolyOutput(address, sendAmount))

      val boxReader =
        MockBoxReader.fromSeq(
          fromAddresses.head.toAddress ->
          Seq(polyBox.copy(value = SimpleValue(polyOutputsAmount(polyOutputs.toList) + fee.data)), assetBox)
        )

      val request =
        TransferRequests.UnprovenTransferRequest(
          fromAddresses.toList,
          polyOutputs.toList,
          fromAddresses.head,
          fromAddresses.head,
          fee,
          None,
          minting = false
        )

      val transferResult = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      transferResult.value.inputs.map(_._2) should not contain assetBox.nonce
    }
  }

  it should "use all available poly boxes when only sending arbits" in {
    forAll(
      dionAddressesGen,
      dionAddressesGen,
      ModelGenerators.arbitraryPositiveInt128.arbitrary,
      ModelGenerators.arbitraryPositiveInt128.arbitrary,
      polyBoxGen,
      arbitBoxGen
    ) { (fromAddresses, toAddresses, fee, sendAmount, polyBox, arbitBox) =>
      val arbitOutputs = toAddresses.map(address => Transaction.ArbitOutput(address, sendAmount))

      val boxReader =
        MockBoxReader.fromSeq(
          fromAddresses.head.toAddress ->
          Seq(
            polyBox.copy(value = SimpleValue(fee.data)),
            arbitBox.copy(value = SimpleValue(arbitOutputsAmount(arbitOutputs.toList)))
          )
        )

      val request =
        TransferRequests.UnprovenTransferRequest(
          fromAddresses.toList,
          arbitOutputs.toList,
          fromAddresses.head,
          fromAddresses.head,
          fee,
          None,
          minting = false
        )

      val transferResult = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      transferResult.value.inputs.map(_._2) should contain(polyBox.nonce)
    }
  }

  it should "have a change output when more polys are supplied than needed" in {
    forAll(
      dionAddressesGen,
      dionAddressesGen,
      ModelGenerators.arbitraryPositiveInt128.arbitrary,
      ModelGenerators.arbitraryPositiveInt128.arbitrary,
      polyBoxGen,
      ModelGenerators.arbitraryPositiveInt128.arbitrary
    ) { (fromAddresses, toAddresses, fee, sendAmount, polyBox, changeAmount) =>
      val polyOutputs = toAddresses.map(address => Transaction.PolyOutput(address, sendAmount))

      val existingPolys = polyOutputsAmount(polyOutputs.toList) + changeAmount.data + fee.data

      val boxReader =
        MockBoxReader.fromSeq(
          fromAddresses.head.toAddress -> Seq(polyBox.copy(value = SimpleValue(existingPolys)))
        )

      val request =
        TransferRequests.UnprovenTransferRequest(
          fromAddresses.toList,
          polyOutputs.toList,
          fromAddresses.head,
          fromAddresses.head,
          fee,
          None,
          minting = false
        )

      val transferResult = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      transferResult.value.feeOutput.value.value shouldBe changeAmount
    }
  }

  it should "have no fee change output when no fee and exact number of polys are provided" in {
    forAll(dionAddressesGen, polyBoxGen, ModelGenerators.arbitraryPolyOutputs.arbitrary) {
      (senders, polyBox, outputs) =>
        val polysSent = polyOutputsAmount(outputs.toList)

        val boxReader =
          MockBoxReader.fromSeq(senders.head.toAddress -> Seq(polyBox.copy(value = SimpleValue(polysSent))))

        val request =
          TransferRequests.UnprovenTransferRequest(
            senders.toList,
            outputs.toList,
            senders.head,
            senders.head,
            zeroInt128,
            None,
            minting = false
          )

        val result = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

        result.value.feeOutput shouldBe None
    }
  }

  it should "have some change output when no fee and more polys are supplied than needed" in {
    forAll(
      dionAddressesGen,
      polyBoxGen,
      ModelGenerators.arbitraryPolyOutputs.arbitrary,
      ModelGenerators.arbitraryPositiveInt128.arbitrary
    ) { (senders, polyBox, outputs, extra) =>
      val polysSent = polyOutputsAmount(outputs.toList)

      val boxReader =
        MockBoxReader.fromSeq(senders.head.toAddress -> Seq(polyBox.copy(value = SimpleValue(polysSent + extra.data))))

      val request =
        TransferRequests.UnprovenTransferRequest(
          senders.toList,
          outputs.toList,
          senders.head,
          senders.head,
          zeroInt128,
          None,
          minting = false
        )

      val result = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      result.value.feeOutput shouldBe Some(Transaction.PolyOutput(senders.head, extra))
    }
  }

  it should "have no fee change output when exact number of polys are supplied for fee and payment" in {
    forAll(
      dionAddressesGen,
      polyBoxGen,
      ModelGenerators.arbitraryPolyOutputs.arbitrary,
      ModelGenerators.arbitraryPositiveInt128.arbitrary
    ) { (senders, polyBox, outputs, fee) =>
      val polysSent = polyOutputsAmount(outputs.toList)

      val boxReader =
        MockBoxReader.fromSeq(
          senders.head.toAddress -> Seq(polyBox.copy(value = SimpleValue(polysSent + fee.data)))
        )

      val request =
        TransferRequests.UnprovenTransferRequest(
          senders.toList,
          outputs.toList,
          senders.head,
          senders.head,
          fee,
          None,
          minting = false
        )

      val result = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      result.value.feeOutput shouldBe None
    }
  }

  it should "have a poly change output and no arbit change output when exact arbits are provided and no fee" in {
    forAll(
      dionAddressesGen,
      ModelGenerators.arbitraryArbitOutputs.arbitrary,
      Gen.zip(polyBoxGen, arbitBoxGen)
    ) { (senders, arbitOutputs, boxes) =>
      val arbitsSent = arbitOutputsAmount(arbitOutputs.toList)

      val boxReader =
        MockBoxReader.fromSeq(
          senders.head.toAddress -> Seq(boxes._1, boxes._2.copy(value = SimpleValue(arbitsSent)))
        )

      val request =
        TransferRequests.UnprovenTransferRequest(
          senders.toList,
          arbitOutputs.toList,
          senders.head,
          senders.head,
          zeroInt128,
          None,
          minting = false
        )

      val result = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      result.value.feeOutput shouldBe Some(Transaction.PolyOutput(senders.head, boxes._1.value.quantity.toSized))
      result.value.coinOutputs.toList should not contain Transaction.ArbitOutput(senders.head, Sized.maxUnsafe(0))
    }
  }

  it should "have a poly change output and no arbit change output when exact arbits are provided and" +
  "more polys are provided than the fee" in {
    forAll(
      dionAddressesGen,
      ModelGenerators.arbitraryArbitOutputs.arbitrary,
      Gen.zip(polyBoxGen, arbitBoxGen),
      ModelGenerators.arbitraryPositiveInt128.arbitrary,
      ModelGenerators.arbitraryPositiveInt128.arbitrary
    ) { (senders, arbitOutputs, boxes, fee, extraPolys) =>
      val arbitsSent = arbitOutputsAmount(arbitOutputs.toList)

      val boxReader =
        MockBoxReader.fromSeq(
          senders.head.toAddress -> Seq(
            boxes._1.copy(value = SimpleValue(fee.data + extraPolys.data)),
            boxes._2.copy(value = SimpleValue(arbitsSent))
          )
        )

      val request =
        TransferRequests.UnprovenTransferRequest(
          senders.toList,
          arbitOutputs.toList,
          senders.head,
          senders.head,
          fee,
          None,
          minting = false
        )

      val result = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      result.value.feeOutput shouldBe Some(Transaction.PolyOutput(senders.head, extraPolys))
      result.value.coinOutputs.toList should not contain Transaction.ArbitOutput(senders.head, Sized.maxUnsafe(0))
    }
  }

  it should "have a poly and arbit change output when more arbits and polys are provided than the fee and payment" in {
    forAll(
      dionAddressesGen,
      ModelGenerators.arbitraryArbitOutputs.arbitrary,
      Gen.zip(polyBoxGen, arbitBoxGen),
      ModelGenerators.arbitraryPositiveInt128.arbitrary,
      ModelGenerators.arbitraryPositiveInt128.arbitrary,
      ModelGenerators.arbitraryPositiveInt128.arbitrary
    ) { (senders, arbitOutputs, boxes, fee, extraPolys, extraArbits) =>
      val arbitsSent = arbitOutputsAmount(arbitOutputs.toList)

      val boxReader =
        MockBoxReader.fromSeq(
          senders.head.toAddress -> Seq(
            boxes._1.copy(value = SimpleValue(fee.data + extraPolys.data)),
            boxes._2.copy(value = SimpleValue(arbitsSent + extraArbits.data))
          )
        )

      val request =
        TransferRequests.UnprovenTransferRequest(
          senders.toList,
          arbitOutputs.toList,
          senders.head,
          senders.head,
          fee,
          None,
          minting = false
        )

      val result = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      result.value.feeOutput shouldBe Some(Transaction.PolyOutput(senders.head, extraPolys))
      result.value.coinOutputs.toList should contain(Transaction.ArbitOutput(senders.head, extraArbits))
    }
  }

  it should "have no poly or arbit change output when exact amounts of arbits and polys are provided for fee and payment" in {
    forAll(
      dionAddressesGen,
      ModelGenerators.arbitraryArbitOutputs.arbitrary,
      Gen.zip(polyBoxGen, arbitBoxGen),
      ModelGenerators.arbitraryPositiveInt128.arbitrary
    ) { (senders, arbitOutputs, boxes, fee) =>
      val arbitsSent = arbitOutputsAmount(arbitOutputs.toList)

      val boxReader =
        MockBoxReader.fromSeq(
          senders.head.toAddress -> Seq(
            boxes._1.copy(value = SimpleValue(fee.data)),
            boxes._2.copy(value = SimpleValue(arbitsSent))
          )
        )

      val request =
        TransferRequests.UnprovenTransferRequest(
          senders.toList,
          arbitOutputs.toList,
          senders.head,
          senders.head,
          fee,
          None,
          minting = false
        )

      val result = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      result.value.feeOutput shouldBe None
      result.value.coinOutputs.toList should not contain Transaction.ArbitOutput(senders.head, Sized.maxUnsafe(0))
    }
  }

  it should "have no poly change output and an arbit change output when exact fee and more arbits than required are provided" in {
    forAll(
      dionAddressesGen,
      ModelGenerators.arbitraryArbitOutputs.arbitrary,
      Gen.zip(polyBoxGen, arbitBoxGen),
      ModelGenerators.arbitraryPositiveInt128.arbitrary,
      ModelGenerators.arbitraryPositiveInt128.arbitrary
    ) { (senders, arbitOutputs, boxes, fee, extraArbits) =>
      val arbitsSent = arbitOutputsAmount(arbitOutputs.toList)

      val boxReader =
        MockBoxReader.fromSeq(
          senders.head.toAddress -> Seq(
            boxes._1.copy(value = SimpleValue(fee.data)),
            boxes._2.copy(value = SimpleValue(arbitsSent + extraArbits.data))
          )
        )

      val request =
        TransferRequests.UnprovenTransferRequest(
          senders.toList,
          arbitOutputs.toList,
          senders.head,
          senders.head,
          fee,
          None,
          minting = false
        )

      val result = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      result.value.feeOutput shouldBe None
      result.value.coinOutputs.toList should contain(Transaction.ArbitOutput(senders.head, extraArbits))
    }
  }

  it should "have a poly change output when minting an asset with no fee" in {
    forAll(
      dionAddressesGen,
      ModelGenerators.arbitraryAssetOutput.arbitrary,
      polyBoxGen
    ) { (senders, assetOutput, polyBox) =>
      val boxReader = MockBoxReader.fromSeq(
        senders.head.toAddress -> Seq(polyBox)
      )

      val request =
        TransferRequests.UnprovenTransferRequest(
          senders.toList,
          List(assetOutput),
          senders.head,
          senders.head,
          zeroInt128,
          None,
          minting = true
        )

      val result = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      result.value.feeOutput.value shouldBe Transaction.PolyOutput(senders.head, polyBox.value.quantity.toSized)
    }
  }

  it should "have a poly change output when minting an asset and more polys are provided than the fee" in {
    forAll(
      dionAddressesGen,
      ModelGenerators.arbitraryAssetOutput.arbitrary,
      polyBoxGen,
      ModelGenerators.arbitraryPositiveInt128.arbitrary,
      ModelGenerators.arbitraryPositiveInt128.arbitrary
    ) { (senders, assetOutput, polyBox, fee, extraPolys) =>
      val boxReader = MockBoxReader.fromSeq(
        senders.head.toAddress -> Seq(polyBox.copy(value = SimpleValue(fee.data + extraPolys.data)))
      )

      val request =
        TransferRequests.UnprovenTransferRequest(
          senders.toList,
          List(assetOutput),
          senders.head,
          senders.head,
          fee,
          None,
          minting = true
        )

      val result = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      result.value.feeOutput.value shouldBe Transaction.PolyOutput(senders.head, extraPolys)
    }
  }

  it should "have a poly change output when sending an exact amount of assets with no fee" in {
    forAll(dionAddressesGen, assetBoxGen, polyBoxGen, ModelGenerators.arbitraryAssetOutput.arbitrary) {
      (senders, assetBox, polyBox, assetOutput) =>
        val boxReader = MockBoxReader.fromSeq(
          senders.head.toAddress -> Seq(polyBox, assetBox.copy(value = assetOutput.value.toAssetValue))
        )

        val request =
          TransferRequests.UnprovenTransferRequest(
            senders.toList,
            List(assetOutput),
            senders.head,
            senders.head,
            zeroInt128,
            None,
            minting = false
          )

        val result = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

        result.value.feeOutput.value shouldBe Transaction.PolyOutput(senders.head, polyBox.value.quantity.toSized)
    }
  }

  it should "have a poly change and asset change output when providing more than required amount of assets with no fee" in {
    forAll(
      dionAddressesGen,
      assetBoxGen,
      polyBoxGen,
      ModelGenerators.arbitraryAssetOutput.arbitrary,
      ModelGenerators.arbitraryPositiveInt128.arbitrary
    ) { (senders, assetBox, polyBox, assetOutput, extraAssets) =>
      val boxReader = MockBoxReader.fromSeq(
        senders.head.toAddress -> Seq(
          polyBox,
          assetBox.copy(value =
            assetOutput.value.toAssetValue.copy(quantity = assetOutput.value.quantity.data + extraAssets.data)
          )
        )
      )

      val expectedAssetChangeOutput =
        Transaction.AssetOutput(
          senders.head,
          TetraBox.Values.Asset(extraAssets, assetOutput.value.assetCode, ByteVector.empty, None)
        )

      val request =
        TransferRequests.UnprovenTransferRequest(
          senders.toList,
          List(assetOutput),
          senders.head,
          senders.head,
          zeroInt128,
          None,
          minting = false
        )

      val result = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      result.value.feeOutput.value shouldBe Transaction.PolyOutput(senders.head, polyBox.value.quantity.toSized)
      result.value.coinOutputs.toList should contain(expectedAssetChangeOutput)
    }
  }

  it should "have a poly change and no asset change when providing exact asset amount with more polys than the fee" in {
    forAll(
      dionAddressesGen,
      assetBoxGen,
      polyBoxGen,
      ModelGenerators.arbitraryAssetOutput.arbitrary,
      ModelGenerators.arbitraryPositiveInt128.arbitrary,
      ModelGenerators.arbitraryPositiveInt128.arbitrary
    ) { (senders, assetBox, polyBox, assetOutput, fee, extraPolys) =>
      val boxReader = MockBoxReader.fromSeq(
        senders.head.toAddress -> Seq(
          polyBox.copy(value = SimpleValue(fee.data + extraPolys.data)),
          assetBox.copy(value = assetOutput.value.toAssetValue)
        )
      )

      val request =
        TransferRequests.UnprovenTransferRequest(
          senders.toList,
          List(assetOutput),
          senders.head,
          senders.head,
          fee,
          None,
          minting = false
        )

      val result = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      result.value.feeOutput.value shouldBe Transaction.PolyOutput(senders.head, extraPolys)
      result.value.coinOutputs.toList should not contain Transaction.AssetOutput(
        senders.head,
        TetraBox.Values.Asset(zeroInt128, assetOutput.value.assetCode, ByteVector.empty, None)
      )
    }
  }

  it should "have a poly and asset change outputs when providing more polys and assets than required" in {
    forAll(
      dionAddressesGen,
      Gen.zip(polyBoxGen, assetBoxGen),
      ModelGenerators.arbitraryAssetOutput.arbitrary,
      ModelGenerators.arbitraryPositiveInt128.arbitrary,
      ModelGenerators.arbitraryPositiveInt128.arbitrary,
      ModelGenerators.arbitraryPositiveInt128.arbitrary
    ) { (senders, boxes, assetOutput, fee, extraPolys, extraAssets) =>
      val boxReader = MockBoxReader.fromSeq(
        senders.head.toAddress -> Seq(
          boxes._1.copy(value = SimpleValue(fee.data + extraPolys.data)),
          boxes._2.copy(value =
            assetOutput.value.toAssetValue.copy(quantity = assetOutput.value.quantity.data + extraAssets.data)
          )
        )
      )

      val expectedAssetChangeOutput =
        Transaction.AssetOutput(
          senders.head,
          TetraBox.Values.Asset(extraAssets, assetOutput.value.assetCode, ByteVector.empty, None)
        )

      val request =
        TransferRequests.UnprovenTransferRequest(
          senders.toList,
          List(assetOutput),
          senders.head,
          senders.head,
          fee,
          None,
          minting = false
        )

      val result = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      result.value.feeOutput.value shouldBe Transaction.PolyOutput(senders.head, extraPolys)
      result.value.coinOutputs.toList should contain(expectedAssetChangeOutput)
    }
  }

  it should "have no change outputs when providing exact assets and polys" in {
    forAll(
      dionAddressesGen,
      Gen.zip(polyBoxGen, assetBoxGen),
      ModelGenerators.arbitraryAssetOutput.arbitrary,
      ModelGenerators.arbitraryPositiveInt128.arbitrary
    ) { (senders, boxes, assetOutput, fee) =>
      val boxReader = MockBoxReader.fromSeq(
        senders.head.toAddress -> Seq(
          boxes._1.copy(value = SimpleValue(fee.data)),
          boxes._2.copy(value = assetOutput.value.toAssetValue)
        )
      )

      val request =
        TransferRequests.UnprovenTransferRequest(
          senders.toList,
          List(assetOutput),
          senders.head,
          senders.head,
          fee,
          None,
          minting = false
        )

      val result = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      result.value.feeOutput shouldBe None
      result.value.coinOutputs.toList should not contain Transaction.AssetOutput(
        senders.head,
        TetraBox.Values.Asset(zeroInt128, assetOutput.value.assetCode, ByteVector.empty, None)
      )
    }
  }

  it should "have an asset change output when providing more assets and exact polys" in {
    forAll(
      dionAddressesGen,
      Gen.zip(polyBoxGen, assetBoxGen),
      ModelGenerators.arbitraryAssetOutput.arbitrary,
      ModelGenerators.arbitraryPositiveInt128.arbitrary,
      ModelGenerators.arbitraryPositiveInt128.arbitrary
    ) { (senders, boxes, assetOutput, fee, extraAssets) =>
      val boxReader = MockBoxReader.fromSeq(
        senders.head.toAddress -> Seq(
          boxes._1.copy(value = SimpleValue(fee.data)),
          boxes._2.copy(value =
            assetOutput.value.toAssetValue.copy(quantity = assetOutput.value.quantity.data + extraAssets.data)
          )
        )
      )

      val expectedAssetChangeOutput =
        Transaction.AssetOutput(
          senders.head,
          TetraBox.Values.Asset(extraAssets, assetOutput.value.assetCode, ByteVector.empty, None)
        )

      val request =
        TransferRequests.UnprovenTransferRequest(
          senders.toList,
          List(assetOutput),
          senders.head,
          senders.head,
          fee,
          None,
          minting = false
        )

      val result = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      result.value.feeOutput shouldBe None
      result.value.coinOutputs.toList should contain(expectedAssetChangeOutput)
    }
  }

  it should "not have a poly change output when minting an asset with exact polys provided for fee" in {
    forAll(
      dionAddressesGen,
      ModelGenerators.arbitraryAssetOutput.arbitrary,
      polyBoxGen,
      ModelGenerators.arbitraryPositiveInt128.arbitrary
    ) { (senders, assetOutput, polyBox, fee) =>
      val boxReader = MockBoxReader.fromSeq(
        senders.head.toAddress -> Seq(polyBox.copy(value = SimpleValue(fee.data)))
      )

      val request =
        TransferRequests.UnprovenTransferRequest(
          senders.toList,
          List(assetOutput),
          senders.head,
          senders.head,
          fee,
          None,
          minting = true
        )

      val result = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      result.value.feeOutput shouldBe None
    }
  }

  it should "be invalid if not enough polys are provided to pay the fee and payment" in {
    forAll(
      dionAddressesGen,
      dionAddressesGen,
      ModelGenerators.arbitraryPositiveInt128.arbitrary.filter(_.data > 0),
      ModelGenerators.arbitraryPositiveInt128.arbitrary.filter(_.data > 0),
      polyBoxGen
    ) { (fromAddresses, toAddresses, fee, sendAmount, polyBox) =>
      val polyOutputs = toAddresses.map(address => Transaction.PolyOutput(address, sendAmount))

      val totalPolysSent = polyOutputsAmount(polyOutputs.toList)

      val boxReader =
        MockBoxReader.fromSeq(
          fromAddresses.head.toAddress ->
          Seq(polyBox.copy(value = SimpleValue(totalPolysSent)))
        )

      val expectedFailure =
        BuildTransferFailures.InsufficientPolyFunds(
          totalPolysSent,
          totalPolysSent + fee.data
        )

      val request =
        TransferRequests.UnprovenTransferRequest(
          fromAddresses.toList,
          polyOutputs.toList,
          fromAddresses.head,
          fromAddresses.head,
          fee,
          None,
          minting = false
        )

      val transferResult = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      transferResult.left.value shouldBe expectedFailure
    }
  }

  it should "be invalid if not enough arbits are provided to pay the outputs" in {
    forAll(
      dionAddressesGen,
      dionAddressesGen,
      ModelGenerators.arbitraryPositiveInt128.arbitrary,
      ModelGenerators.arbitraryPositiveInt128.arbitrary.filter(_.data > 0),
      polyBoxGen,
      arbitBoxGen
    ) { (fromAddresses, toAddresses, fee, sendAmount, polyBox, arbitBox) =>
      val arbitOutputs = toAddresses.map(address => Transaction.ArbitOutput(address, sendAmount))

      val totalArbitsSent = arbitOutputsAmount(arbitOutputs.toList)

      val existingArbits = totalArbitsSent - 1

      val boxReader =
        MockBoxReader.fromSeq(
          fromAddresses.head.toAddress ->
          Seq(arbitBox.copy(value = SimpleValue(existingArbits)), polyBox)
        )

      val expectedFailure =
        BuildTransferFailures.InsufficientArbitFunds(
          existingArbits,
          totalArbitsSent
        )

      val request =
        TransferRequests.UnprovenTransferRequest(
          fromAddresses.toList,
          arbitOutputs.toList,
          fromAddresses.head,
          fromAddresses.head,
          fee,
          None,
          minting = false
        )

      val transferResult = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      transferResult.left.value shouldBe expectedFailure
    }
  }

  it should "be invalid if no poly boxes are provided as inputs" in {
    forAll(
      arbitBoxesGen,
      dionAddressesGen,
      dionAddressesGen,
      ModelGenerators.arbitraryPositiveInt128.arbitrary,
      ModelGenerators.arbitraryPositiveInt128.arbitrary
    ) { (arbitBoxes, senders, recipients, amount, fee) =>
      val arbitOutputs = recipients.map(recipient => Transaction.ArbitOutput(recipient, amount))

      val totalArbitsSent = arbitOutputsAmount(arbitOutputs.toList)

      val arbitBoxesToUse =
        NonEmptyChain
          .one(arbitBoxes.head.copy(value = SimpleValue(totalArbitsSent)))
          .appendChain(arbitBoxes.tail)

      val boxReader = MockBoxReader.fromNec(senders.head.toAddress -> arbitBoxesToUse)

      val request =
        TransferRequests.UnprovenTransferRequest(
          senders.toList,
          arbitOutputs.toList,
          senders.head,
          senders.head,
          fee,
          None,
          minting = false
        )

      val transferResult = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      transferResult.left.value shouldBe BuildTransferFailures.EmptyPolyInputs
    }
  }

  it should "be invalid if no outputs are provided" in {
    forAll(
      polyBoxesGen,
      dionAddressesGen,
      ModelGenerators.arbitraryPositiveInt128.arbitrary
    ) { (polyBoxes, senders, fee) =>
      val boxReader =
        MockBoxReader.fromSeq(senders.head.toAddress -> polyBoxes.toList)

      val request =
        TransferRequests.UnprovenTransferRequest(
          senders.toList,
          List.empty,
          senders.head,
          senders.head,
          fee,
          None,
          minting = false
        )

      val transferResult = TransferBuilder.buildUnprovenTransfer(boxReader, request, BoxSelectionAlgorithms.All)

      transferResult.left.value shouldBe BuildTransferFailures.EmptyOutputs
    }
  }
}
