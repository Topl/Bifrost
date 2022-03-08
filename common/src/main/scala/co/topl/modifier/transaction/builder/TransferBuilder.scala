package co.topl.modifier.transaction.builder

import cats.data.{Chain, NonEmptyChain}
import cats.implicits._
import co.topl.attestation.ops.implicits._
import co.topl.attestation.{Address, EvidenceProducer, Proposition}
import co.topl.models.Box.Values.Asset
import co.topl.models.utility.HasLength.instances.latin1DataLength
import co.topl.models.utility.StringDataTypes.{Latin1Data => TetraLatin1Data}
import co.topl.models.utility.{Lengths, Sized}
import co.topl.models.{Box => TetraBox, BoxReference, Bytes, Transaction}
import co.topl.modifier.box._
import co.topl.modifier.implicits._
import co.topl.modifier.transaction.builder.Validation._
import co.topl.modifier.transaction.builder.ops.BoxSetOps.ToBoxReferencesFailures
import co.topl.modifier.transaction.builder.ops.implicits._
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer}
import co.topl.modifier.{BoxReader, ProgramId}
import co.topl.utils.ops.implicits._
import co.topl.utils.{Identifiable, Int128}

import java.time.Instant
import scala.collection.immutable.ListMap

object TransferBuilder {

  /**
   * Builds an unsigned poly transfer from a box state, a request, and an algorithm for box selection.
   * @param boxReader the state of UTXO boxes
   * @param request the `PolyTransferRequest` to build an unsigned TX from
   * @param boxSelection the selection algorithm for choosing which boxes should be transaction inputs
   * @tparam P the proposition type
   * @return an unsigned `PolyTransfer` transaction if successful, or a `BuildTransferFailure` if an error occurred
   */
  def buildUnsignedPolyTransfer[P <: Proposition: Identifiable: EvidenceProducer](
    boxReader:    BoxReader[ProgramId, Address],
    request:      TransferRequests.PolyTransferRequest,
    boxSelection: BoxSelectionAlgorithm
  ): Either[BuildTransferFailure, PolyTransfer[P]] = {
    val availableBoxes = getAvailableBoxes(request.from, boxReader)

    val polysOwed = request.to.map(_._2).sum

    // filter the available boxes as specified in the box selection algorithm
    val filteredBoxes = BoxSelectionAlgorithm.pickBoxes(boxSelection, availableBoxes, polysOwed, 0, Map.empty)
    val inputPolys = filteredBoxes.polys

    val inputNonces = inputPolys.map(_._2.nonce)
    val outputAddresses = getOutputAddresses(request.to)

    val polyFunds = boxFunds(inputPolys)
    val polysAvailableAfterFee = polyFunds - request.fee

    val changeAmount = polyFunds - request.fee - polysOwed

    val polyOutputs = request.to.map(x => x._1 -> SimpleValue(x._2))

    val polyChangeOutput = request.changeAddress -> SimpleValue(changeAmount)

    val inputs = inputPolys.map(box => box._1 -> box._2.nonce).toIndexedSeq
    val outputs = (polyChangeOutput :: polyOutputs).toIndexedSeq

    val polyTransfer =
      PolyTransfer[P](
        inputs,
        outputs,
        ListMap(),
        request.fee,
        Instant.now.toEpochMilli,
        request.data,
        minting = false
      )

    for {
      _ <- validateNonEmptyInputNonces(inputNonces)
      _ <- validateUniqueInputNonces(inputNonces)
      _ <- validateNonEmptyOutputAddresses(outputAddresses)
      _ <- validateUniqueOutputAddresses(outputAddresses)
      _ <- validateFeeFunds(polyFunds, request.fee)
      _ <- validatePaymentFunds(polysAvailableAfterFee, polysOwed)
    } yield polyTransfer
  }

  /**
   * Builds an unsigned asset transfer from a box state, a request, and an algorithm for box selection.
   * @param boxReader the state of UTXO boxes
   * @param request the `PolyTransferRequest` to build an unsigned TX from
   * @param boxSelection the selection algorithm for choosing which boxes should be inputs
   * @tparam P the proposition type
   * @return an unsigned `PolyTransfer` transaction if successful, or a `BuildTransferFailure` if an error occurred
   */
  def buildUnsignedAssetTransfer[P <: Proposition: Identifiable: EvidenceProducer](
    boxReader:    BoxReader[ProgramId, Address],
    request:      TransferRequests.AssetTransferRequest,
    boxSelection: BoxSelectionAlgorithm
  ): Either[BuildTransferFailure, AssetTransfer[P]] = {
    val availableBoxes = getAvailableBoxes(request.from, boxReader)

    val assetsNeeded = request.to.groupMapReduce(_._2.assetCode)(_._2.quantity)(_ + _)

    // filter the available boxes as specified in the box selection algorithm
    val filteredBoxes =
      BoxSelectionAlgorithm.pickBoxes(boxSelection, availableBoxes, request.fee, 0, assetsNeeded)
    val inputPolys = filteredBoxes.polys
    val inputAssets = filteredBoxes.assets

    val outputAddresses = getOutputAddresses(request.to)

    val polyInputNonces = inputPolys.map(_._2.nonce)
    val assetInputNonces = inputAssets.map(_._2.nonce)

    val polyFunds = boxFunds(inputPolys)
    val assetFunds = boxFunds(inputAssets)

    val assetCodeOpt = request.to.headOption.map(_._2.assetCode)

    val polyChange = polyFunds - request.fee

    val assetsOwed = assetsNeeded.values.sum

    val assetChange =
      // no input assets required when minting
      if (!request.minting) assetFunds - assetsOwed
      else Int128(0)

    val polyChangeOutput = request.changeAddress -> SimpleValue(polyChange)

    val inputs = (inputPolys ++ inputAssets).map(input => input._1 -> input._2.nonce).toIndexedSeq

    val outputs: AssetCode => IndexedSeq[(Address, TokenValueHolder)] = { assetCode =>
      val assetChangeOutput = request.consolidationAddress -> AssetValue(assetChange, assetCode)

      (polyChangeOutput :: assetChangeOutput :: request.to).toIndexedSeq
    }

    val assetTransfer: AssetCode => AssetTransfer[P] = assetCode =>
      AssetTransfer[P](
        inputs,
        outputs(assetCode),
        ListMap(),
        request.fee,
        Instant.now.toEpochMilli,
        request.data,
        request.minting
      )

    for {
      _ <- validateNonEmptyInputNonces(polyInputNonces)
      _ <- validateNonEmptyInputNonces(assetInputNonces)
      _ <- validateUniqueInputNonces(polyInputNonces)
      _ <- validateUniqueInputNonces(assetInputNonces)
      _ <- validateNonEmptyOutputAddresses(outputAddresses)
      _ <- validateUniqueOutputAddresses(outputAddresses)
      // safe because we have checked that the outputs are not empty
      assetCode = assetCodeOpt.get
      _ <-
        Option
          .when(!request.minting)(validateSameAssetCode(assetCode, inputAssets.map(_._2)))
          .getOrElse(Right(assetCode))
      _ <- validateFeeFunds(polyFunds, request.fee)
      _ <-
        Option.when(!request.minting)(validatePaymentFunds(assetFunds, assetsOwed)).getOrElse(Right(assetFunds))
    } yield assetTransfer(assetCode)
  }

  /**
   * Builds an unsigned arbit transfer from a box state, a request, and an algorithm for box selection.
   * @param boxReader the state of UTXO boxes
   * @param request the `AssetTransferRequest` to build an unsigned TX from
   * @param boxSelection the selection algorithm for choosing which boxes should be transaction inputs
   * @tparam P the proposition type
   * @return an unsigned `AssetTransfer` transaction if successful, or a `BuildTransferFailure` if an error occurred
   */
  def buildUnsignedArbitTransfer[P <: Proposition: Identifiable: EvidenceProducer](
    boxReader:    BoxReader[ProgramId, Address],
    request:      TransferRequests.ArbitTransferRequest,
    boxSelection: BoxSelectionAlgorithm
  ): Either[BuildTransferFailure, ArbitTransfer[P]] = {
    val availableBoxes = getAvailableBoxes(request.from, boxReader)

    val arbitsOwed = request.to.map(_._2).sum

    // filter the available boxes as specified in the box selection algorithm
    val filteredBoxes =
      BoxSelectionAlgorithm.pickBoxes(boxSelection, availableBoxes, request.fee, arbitsOwed, Map.empty)
    val inputPolys = filteredBoxes.polys
    val inputArbits = filteredBoxes.arbits

    val inputPolyNonces = inputPolys.map(_._2.nonce)
    val inputArbitNonces = inputArbits.map(_._2.nonce)

    val outputAddresses = getOutputAddresses(request.to)

    val polyFunds = boxFunds(inputPolys)
    val arbitFunds = boxFunds(inputArbits)

    val polyChange = polyFunds - request.fee
    val arbitChange = arbitFunds - arbitsOwed

    val arbitOutputs = request.to.map(x => x._1 -> SimpleValue(x._2))
    val polyChangeOutput = request.changeAddress         -> SimpleValue(polyChange)
    val arbitChangeOutput = request.consolidationAddress -> SimpleValue(arbitChange)

    val inputs = (inputPolys ++ inputArbits).map(x => x._1 -> x._2.nonce).toIndexedSeq
    val outputs = (polyChangeOutput :: arbitChangeOutput :: arbitOutputs).toIndexedSeq

    val arbitTransfer =
      ArbitTransfer[P](
        inputs,
        outputs,
        ListMap(),
        request.fee,
        Instant.now.toEpochMilli,
        request.data,
        minting = false
      )

    for {
      _ <- validateNonEmptyInputNonces(inputPolyNonces)
      _ <- validateUniqueInputNonces(inputPolyNonces)
      _ <- validateNonEmptyInputNonces(inputArbitNonces)
      _ <- validateUniqueInputNonces(inputArbitNonces)
      _ <- validateNonEmptyOutputAddresses(outputAddresses)
      _ <- validateUniqueOutputAddresses(outputAddresses)
      _ <- validateFeeFunds(polyFunds, request.fee)
      _ <- validatePaymentFunds(arbitFunds, arbitsOwed)
    } yield arbitTransfer
  }

  def buildUnprovenTransfer(
    boxReader:    BoxReader[ProgramId, Address],
    request:      TransferRequests.UnprovenTransferRequest,
    boxSelection: BoxSelectionAlgorithm
  ): Either[BuildTransferFailure, Transaction.Unproven] = {
    val availableBoxes = getAvailableBoxes(request.from.map(_.toAddress), boxReader)

    val (polyOutputs, arbitOutputs, assetOutputs) = request.to.splitByCoinType

    val polysOwed = polyOutputs.map(_.value.data).sum
    val arbitsOwed = arbitOutputs.map(_.value.data).sum
    val assetsOwed =
      assetOutputs.groupMapReduce(_.value.toAssetValue.assetCode)(value => Int128(value.value.quantity.data))(_ + _)

    val filteredBoxes =
      BoxSelectionAlgorithm.pickBoxes(boxSelection, availableBoxes, polysOwed, arbitsOwed, assetsOwed)

    val polyInputNonces = filteredBoxes.polys.map(_._2.nonce)

    val polyChange = filteredBoxes.polySum - Int128(request.fee.data) - polysOwed
    val arbitChange = filteredBoxes.arbitSum - arbitsOwed

    val assetChange: Map[AssetCode, Int128] =
      filteredBoxes.assetSum
        .map(asset => asset._1 -> (asset._2 - assetsOwed.getOrElse(asset._1, 0)))

    val polyChangeOutput =
      Option.when(polyChange > 0)(Transaction.PolyOutput(request.feeChangeAddress, polyChange.toSized))
    val arbitChangeOutput = Transaction.ArbitOutput(request.consolidationAddress, arbitChange.toSized)

    val assetChangeOutputsResult =
      assetChange.toList
        .traverse(asset =>
          (
            asset._1.issuer.toDionAddress.leftMap(_ => BuildTransferFailures.InvalidAddress(asset._1.issuer)),
            Sized
              .max[TetraLatin1Data, Lengths.`8`.type](
                TetraLatin1Data.fromData(asset._1.shortName.value)
              )
              .leftMap(_ => BuildTransferFailures.InvalidShortName(asset._1.shortName))
          )
            .mapN((issuer, shortName) =>
              Asset.Code(
                asset._1.version,
                issuer,
                shortName
              )
            )
            .map(assetCode =>
              Transaction.AssetOutput(
                request.consolidationAddress,
                TetraBox.Values.Asset(
                  asset._2.toSized,
                  assetCode,
                  Bytes.empty,
                  None
                )
              )
            )
        )

    val transfer: (List[BoxReference], List[Transaction.AssetOutput]) => Transaction.Unproven =
      (boxReferences, assetChangeOutputs) =>
        Transaction.Unproven(
          boxReferences,
          polyChangeOutput,
          NonEmptyChain
            .one(arbitChangeOutput)
            .appendChain(Chain.fromSeq(assetChangeOutputs))
            .appendChain(Chain.fromSeq(request.to)),
          request.fee,
          Instant.now.toEpochMilli,
          request.data,
          request.minting
        )

    for {
      _                  <- validateNonEmptyInputNonces(polyInputNonces)
      assetChangeOutputs <- assetChangeOutputsResult
      boxReferences <-
        filteredBoxes.toBoxReferences.leftMap { case ToBoxReferencesFailures.InvalidAddress(address) =>
          BuildTransferFailures.InvalidAddress(address)
        }
      _ <- validateNonEmptyOutputAddresses(boxReferences.map(_._1.toAddress))
    } yield transfer(boxReferences, assetChangeOutputs)
  }

  /**
   * Gets the available boxes a list of addresses owns.
   * @param addresses the list of addresses to get boxes for
   * @param state the current state of unopened boxes
   * @return a set of available boxes a `TokenBoxes` type
   */
  private def getAvailableBoxes(
    addresses: List[Address],
    state:     BoxReader[ProgramId, Address]
  ): BoxSet =
    addresses
      .flatMap(addr =>
        state
          .getTokenBoxes(addr)
          .getOrElse(List())
          .map(addr -> _)
      )
      .foldLeft(BoxSet.empty) {
        case (boxes, (addr, box: PolyBox))  => boxes.copy(polys = (addr -> box) :: boxes.polys)
        case (boxes, (addr, box: ArbitBox)) => boxes.copy(arbits = (addr -> box) :: boxes.arbits)
        case (boxes, (addr, box: AssetBox)) => boxes.copy(assets = (addr -> box) :: boxes.assets)
        case (boxes, _)                     => boxes
      }

  /**
   * Calculates the value of funds contained inside of the provided boxes.
   * @param fromBoxes a list of address/box tuples to sum together
   * @return the amount of funds contained within the boxes
   */
  private def boxFunds(fromBoxes: List[(Address, Box[TokenValueHolder])]): Int128 =
    fromBoxes.map(_._2.value.quantity).sum

  private def getOutputAddresses[T](outputs: List[(Address, T)]): List[Address] = outputs.map(_._1)
}
