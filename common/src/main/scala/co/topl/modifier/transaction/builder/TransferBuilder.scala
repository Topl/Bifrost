package co.topl.modifier.transaction.builder

import cats.data.{Chain, NonEmptyChain}
import cats.implicits._
import co.topl.attestation.ops.implicits._
import co.topl.attestation.{Address, EvidenceProducer, Proposition}
import co.topl.models.Box.Values.Asset
import co.topl.models.utility.HasLength.instances.latin1DataLength
import co.topl.models.utility.StringDataTypes.{Latin1Data => TetraLatin1Data}
import co.topl.models.utility.{Lengths, Sized}
import co.topl.models.{Box => TetraBox, BoxReference, Bytes, DionAddress, Transaction}
import co.topl.modifier.box._
import co.topl.modifier.implicits._
import co.topl.modifier.ops.AssetCodeOps.ToTetraAssetCodeFailures
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
    val polysOwed = request.to.map(_._2).sum

    val inputBoxes: BoxSet = pickPolyAndArbitBoxesFromState(request.from, polysOwed, 0, boxSelection, boxReader)

    val outputAddresses = request.to.map(_._1)

    val polyFunds = inputBoxes.polySum

    val changeAmount = polyFunds - request.fee - polysOwed

    val polyChangeOutput = request.changeAddress -> SimpleValue(changeAmount)

    val inputs = inputBoxes.polys.map(box => box._1 -> box._2.nonce).toIndexedSeq

    val outputs = (polyChangeOutput :: request.to.map(x => x._1 -> SimpleValue(x._2))).toIndexedSeq

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
      _ <- validateNonEmptyPolyInputNonces(inputBoxes.polyNonces)
      _ <- validateUniqueInputNonces(inputBoxes.polyNonces)
      _ <- validateNonEmptyOutputAddresses(outputAddresses)
      _ <- validateUniqueOutputAddresses(outputAddresses)
      _ <- validatePositiveOutputValues(request.to.map(_._2))
      _ <- validatePolyFunds(polyFunds, request.fee, polysOwed)
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
    val assetsOwed = request.to.groupMapReduce(_._2.assetCode)(_._2.quantity)(_ + _)

    val inputBoxes =
      if (!request.minting)
        pickBoxesFromState(request.from, request.fee, 0, assetsOwed, boxSelection, boxReader)
      else
        pickPolyAndArbitBoxesFromState(request.from, request.fee, 0, boxSelection, boxReader)

    val outputAddresses = request.to.map(_._1)

    val polyFunds = inputBoxes.polySum

    val assetCodeOpt = request.to.headOption.map(_._2.assetCode)

    val inputs = (inputBoxes.assets.map(_.map(_.nonce)) ++ inputBoxes.polys.map(_.map(_.nonce))).toIndexedSeq

    val outputs: AssetCode => IndexedSeq[(Address, TokenValueHolder)] = { assetCode =>
      val polyChange = polyFunds - request.fee
      val assetChange = inputBoxes.assetSums.values.sum - assetsOwed.values.sum

      val polyChangeOutput = request.changeAddress         -> SimpleValue(polyChange)
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
      _ <- validateNonEmptyPolyInputNonces(inputBoxes.polyNonces)
      _ <- validateUniqueInputNonces(inputBoxes.polyNonces)
      _ <- validateUniqueInputNonces(inputBoxes.assetNonces)
      _ <- validateNonEmptyOutputAddresses(outputAddresses)
      _ <- validateUniqueOutputAddresses(outputAddresses)
      _ <- validatePositiveOutputValues(request.to.map(_._2.quantity))
      // safe because we have checked that the outputs are not empty
      assetCode = assetCodeOpt.get
      _ <- validateSameAssetCode(assetCode, inputBoxes.assets.map(_._2))
      _ <- validatePolyFunds(polyFunds, request.fee, 0)
      _ <-
        // only need to validate asset funds when not a minting transfer
        if (!request.minting)
          validateAssetFunds(
            inputBoxes.assets
              .map(_._2.value.quantity)
              .map(assetCode -> _)
              .toMap,
            assetsOwed
          )
        else Map.empty.asRight
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
    val arbitsOwed = request.to.map(_._2).sum

    val inputBoxes = pickPolyAndArbitBoxesFromState(request.from, request.fee, arbitsOwed, boxSelection, boxReader)

    val outputAddresses = request.to.map(_._1)

    val polyFunds = inputBoxes.polySum
    val arbitFunds = inputBoxes.arbitSum

    val arbitOutputs = request.to.map(x => x._1 -> SimpleValue(x._2))

    val polyChangeOutput = request.changeAddress         -> SimpleValue(polyFunds - request.fee)
    val arbitChangeOutput = request.consolidationAddress -> SimpleValue(arbitFunds - arbitsOwed)

    val polyInputs = inputBoxes.polys.map(_.map(_.nonce))
    val arbitInputs = inputBoxes.arbits.map(_.map(_.nonce))
    val inputs = (polyInputs ++ arbitInputs).toIndexedSeq

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
      _ <- validateNonEmptyPolyInputNonces(inputBoxes.polyNonces)
      _ <- validateUniqueInputNonces(inputBoxes.polyNonces)
      _ <- validateUniqueInputNonces(inputBoxes.arbitNonces)
      _ <- validateNonEmptyOutputAddresses(outputAddresses)
      _ <- validateUniqueOutputAddresses(outputAddresses)
      _ <- validatePositiveOutputValues(request.to.map(_._2))
      _ <- validatePolyFunds(polyFunds, request.fee, 0)
      _ <- validateArbitFunds(arbitFunds, arbitsOwed)
    } yield arbitTransfer
  }

  def buildUnprovenTransfer(
    boxReader:    BoxReader[ProgramId, Address],
    request:      TransferRequests.UnprovenTransferRequest,
    boxSelection: BoxSelectionAlgorithm
  ): Either[BuildTransferFailure, Transaction.Unproven] = {
    val (polyOutputs, arbitOutputs, assetOutputs) = request.to.splitByCoinType

    val polyOutputValues = polyOutputs.map(x => Int128(x.value.data))
    val polysOwed = polyOutputValues.sum

    val arbitOutputValues = arbitOutputs.map(x => Int128(x.value.data))
    val arbitsOwed = arbitOutputValues.sum

    val assetOutputValues = assetOutputs.map(x => Int128(x.value.quantity.data))
    val assetsOwed =
      assetOutputs
        .groupMapReduce(key => key.value.toAssetValue.assetCode)(value => Int128(value.value.quantity.data))(_ + _)

    val inputAddresses = request.from.map(_.toAddress)

    val inputBoxes =
      if (!request.minting)
        pickBoxesFromState(inputAddresses, polysOwed, arbitsOwed, assetsOwed, boxSelection, boxReader)
      else
        pickPolyAndArbitBoxesFromState(inputAddresses, polysOwed, arbitsOwed, boxSelection, boxReader)

    val inputPolyNonces = inputBoxes.polyNonces

    val polyFunds = inputBoxes.polySum
    val polyChange = polyFunds - Int128(request.fee.data) - polysOwed
    val polyChangeOutput =
      Option.when(polyChange > 0)(Transaction.PolyOutput(request.feeChangeAddress, polyChange.toSized))

    val arbitFunds = Option.when(arbitsOwed > 0)(inputBoxes.arbitSum)
    val arbitChange = arbitFunds.map(_ - arbitsOwed)
    val arbitChangeOutput =
      arbitChange.flatMap(change =>
        Option.when(change > 0)(Transaction.ArbitOutput(request.consolidationAddress, change.toSized))
      )

    val assetFunds = inputBoxes.assetSums.filter(_._2 > 0)
    val assetChange: Map[AssetCode, Int128] =
      assetFunds
        .map(asset => asset._1 -> (asset._2 - assetsOwed.getOrElse(asset._1, 0)))
        .filter(_._2 > 0)

    val transfer
      : (List[BoxReference], List[Transaction.AssetOutput]) => Either[BuildTransferFailure, Transaction.Unproven] =
      (boxReferences, assetChangeOutputs) =>
        NonEmptyChain
          .fromSeq(request.to)
          .toRight(BuildTransferFailures.EmptyOutputs)
          .map(outputs => outputs.prependChain(Chain.fromSeq(assetChangeOutputs)))
          .map(outputs => outputs.prependChain(Chain.fromOption(arbitChangeOutput)))
          .map(outputs =>
            Transaction.Unproven(
              boxReferences,
              polyChangeOutput,
              outputs,
              request.fee,
              Instant.now.toEpochMilli,
              request.data,
              request.minting
            )
          )

    for {
      assetChangeOutputs <- toAssetChangeOutput(assetChange, request.consolidationAddress)
      boxReferences      <-
        // do not use arbit boxes if no arbit outputs
        if (arbitsOwed > 0) toBoxReferencesResult(inputBoxes)
        else toBoxReferencesResult(inputBoxes.copy(arbits = List.empty))
      _ <- validateNonEmptyPolyInputNonces(inputPolyNonces)
      _ <- validateUniqueInputNonces(inputPolyNonces)
      _ <- validatePositiveOutputValues(polyOutputValues)
      _ <- validatePositiveOutputValues(arbitOutputValues)
      _ <- validatePositiveOutputValues(assetOutputValues)
      _ <- validatePolyFunds(polyFunds, request.fee.data, polysOwed)
      _ <- arbitFunds.fold(Int128(0).asRight[BuildTransferFailure])(funds => validateArbitFunds(funds, arbitsOwed))
      _ <-
        if (!request.minting) validateAssetFunds(assetFunds, assetsOwed)
        else Map.empty.asRight
      result <- transfer(boxReferences, assetChangeOutputs)
    } yield result
  }

  /**
   * Gets the available boxes a list of addresses owns.
   * @param addresses the list of addresses to get boxes for
   * @param state the current state of unopened boxes
   * @return a set of available boxes a `TokenBoxes` type
   */
  private def pickBoxesFromState(
    addresses:    List[Address],
    polysNeeded:  Int128,
    arbitsNeeded: Int128,
    assetsNeeded: Map[AssetCode, Int128],
    boxAlgorithm: BoxSelectionAlgorithm,
    state:        BoxReader[ProgramId, Address]
  ): BoxSet = {
    val boxesFromState =
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

    BoxSelectionAlgorithm.pickBoxes(boxAlgorithm, boxesFromState, polysNeeded, arbitsNeeded, assetsNeeded)
  }

  private def pickPolyAndArbitBoxesFromState(
    addresses:    List[Address],
    polysNeeded:  Int128,
    arbitsNeeded: Int128,
    boxSelection: BoxSelectionAlgorithm,
    state:        BoxReader[ProgramId, Address]
  ): BoxSet =
    pickBoxesFromState(addresses, polysNeeded, arbitsNeeded, Map.empty, boxSelection, state)

  private def toAssetChangeOutput(
    assetChange:          Map[AssetCode, Int128],
    consolidationAddress: DionAddress
  ): Either[BuildTransferFailure, List[Transaction.AssetOutput]] =
    assetChange.toList
      .traverse(asset =>
        asset._1.toTetraAssetCode
          .map(assetCode =>
            Transaction.AssetOutput(
              consolidationAddress,
              TetraBox.Values.Asset(asset._2.toSized, assetCode, Bytes.empty, None)
            )
          )
          .leftMap {
            case ToTetraAssetCodeFailures.InvalidShortName(shortName) =>
              BuildTransferFailures.InvalidShortName(shortName)
            case ToTetraAssetCodeFailures.InvalidAddress(address) =>
              BuildTransferFailures.InvalidAddress(address)
          }
      )

  private def toBoxReferencesResult(set: BoxSet): Either[BuildTransferFailure, List[BoxReference]] =
    set.toBoxReferences.leftMap { case ToBoxReferencesFailures.InvalidAddress(address) =>
      BuildTransferFailures.InvalidAddress(address)
    }
}
