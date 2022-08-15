package co.topl.modifier.transaction.builder

import cats.implicits._
import co.topl.attestation.{Address, EvidenceProducer, Proposition}
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Sized
import co.topl.models.{Box => TetraBox, Bytes, FullAddress, Transaction}
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

    // run validation and if successful return the created transfer
    for {
      _ <- validateNonEmptyPolyInputNonces(inputBoxes.polyNonces)
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
    val assetsOwed =
      request.to
        .map(_._2)
        .map(value => value.assetCode -> value.quantity)
        .toMap

    val inputBoxes =
      if (!request.minting)
        pickBoxesFromState(request.from, request.fee, 0, assetsOwed, boxSelection, boxReader)
      else
        pickPolyAndArbitBoxesFromState(request.from, request.fee, 0, boxSelection, boxReader)

    val outputAddresses = request.to.map(_._1)

    val polyFunds = inputBoxes.polySum

    val assetCodeOpt = request.to.headOption.map(_._2.assetCode)

    val inputs =
      (inputBoxes.assets.map(a => a._1 -> a._2.nonce) ++ inputBoxes.polys.map(p => p._1 -> p._2.nonce)).toIndexedSeq

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

    // run validation and if successful return the created transfer using the extracted asset code
    for {
      _ <- validateNonEmptyPolyInputNonces(inputBoxes.polyNonces)
      _ <- validateNonEmptyOutputAddresses(outputAddresses)
      _ <- validateUniqueOutputAddresses(outputAddresses)
      _ <- validatePositiveOutputValues(request.to.map(_._2.quantity))
      // safe because we have checked that the outputs are not empty
      assetCode = assetCodeOpt.get
      _ <- validateSameAssetCode(assetCode, inputBoxes.assets.map(_._2).toList)
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

    val polyInputs = inputBoxes.polys.map(p => p._1 -> p._2.nonce)
    val arbitInputs = inputBoxes.arbits.map(a => a._1 -> a._2.nonce)
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

    // run validation and if successful return the created transfer
    for {
      _ <- validateNonEmptyPolyInputNonces(inputBoxes.polyNonces)
      _ <- validateNonEmptyOutputAddresses(outputAddresses)
      _ <- validateUniqueOutputAddresses(outputAddresses)
      _ <- validatePositiveOutputValues(request.to.map(_._2))
      _ <- validatePolyFunds(polyFunds, request.fee, 0)
      _ <- validateArbitFunds(arbitFunds, arbitsOwed)
      // determine the amount of change from each asset used in the transferFunds(arbitFunds, arbitsOwed)
    } yield arbitTransfer
  }

  /**
   * Builds a Tetra Unproven Transfer Transaction from a box state, transfer parameters, and a selection algorithm.
   * @param boxReader the box state to use for selecting inputs to the transfer
   * @param request a set of parameters used to create the transfer
   * @param boxSelection the selection algorithm to use for choosing which boxes from state should be used in the
   *                     transfer transaction
   * @return if successful, a [[Transaction.Unproven]], otherwise a [[BuildTransferFailure]]
   */
  def buildUnprovenTransfer(
    boxReader:    BoxReader[ProgramId, Address],
    request:      TransferRequests.UnprovenTransferRequest,
    boxSelection: BoxSelectionAlgorithm
  ): Either[BuildTransferFailure, Transaction.Unproven] = ???
//  {
//    val (polyOutputs, arbitOutputs, assetOutputs) = request.to.splitByCoinType
//
//    val polyOutputValues = polyOutputs.map(x => Int128(x.value.data))
//    val polysOwed = polyOutputValues.sum
//
//    val arbitOutputValues = arbitOutputs.map(x => Int128(x.value.data))
//    val arbitsOwed = arbitOutputValues.sum
//
//    val assetOutputValues = assetOutputs.map(x => Int128(x.value.quantity.data))
//    val assetsOwed: Map[AssetCode, Int128] =
//      assetOutputs
//        .map(_.value.toAssetValue)
//        .map(value => value.assetCode -> value.quantity)
//        .toMap
//
//    val inputAddresses = request.from.map(_.toAddress)
//
//    // depending on if this is an asset minting transfer, choose to ignore existing asset boxes in state
//    val inputBoxes =
//      if (!request.minting)
//        pickBoxesFromState(inputAddresses, polysOwed, arbitsOwed, assetsOwed, boxSelection, boxReader)
//      else
//        pickPolyAndArbitBoxesFromState(inputAddresses, polysOwed, arbitsOwed, boxSelection, boxReader)
//
//    val inputPolyNonces = inputBoxes.polyNonces
//
//    val polyFunds = inputBoxes.polySum
//    val polyChange = polyFunds - Int128(request.fee.data) - polysOwed
//
//    // only create a poly change output when the poly change is not 0
//    val polyChangeOutput =
//      (polyChange > 0).option(Transaction.PolyOutput(request.feeChangeAddress, polyChange.toSized))
//
//    val arbitFunds = (arbitsOwed > 0).option(inputBoxes.arbitSum)
//    val arbitChange = arbitFunds.map(_ - arbitsOwed)
//
//    // only create an arbit change output when arbits a spent in the transfer and the remaining change is not 0
//    val arbitChangeOutput =
//      arbitChange.flatMap(change =>
//        (change > 0).option(Transaction.ArbitOutput(request.consolidationAddress, change.toSized))
//      )
//
//    val assetFunds = inputBoxes.assetSums.filter(_._2 > 0)
//
//    // determine the amount of change from each asset used in the transfer and ignore if the change amount is 0
//    val assetChange: Map[AssetCode, Int128] =
//      assetFunds
//        .map(asset => asset._1 -> (asset._2 - assetsOwed.getOrElse(asset._1, 0)))
//        .filter(_._2 > 0)
//
//    // create the transfer with a given set of inputs and optional asset change outputs
//    val transfer
//      : (Set[BoxReference], List[Transaction.AssetOutput]) => Either[BuildTransferFailure, Transaction.Unproven] =
//      (boxReferences, assetChangeOutputs) =>
//        NonEmptyChain
//          .fromSeq(request.to)
//          .toRight(BuildTransferFailures.EmptyOutputs)
//          .map(outputs => outputs.prependChain(Chain.fromSeq(assetChangeOutputs)))
//          .map(outputs => outputs.prependChain(Chain.fromOption(arbitChangeOutput)))
//          .map(outputs =>
//            Transaction.Unproven(
//              boxReferences.toList,
//              polyChangeOutput,
//              outputs,
//              request.fee,
//              Instant.now.toEpochMilli,
//              request.data,
//              request.minting
//            )
//          )
//
//    // run validation and create the transfer with the valid parameters
//    for {
//      assetChangeOutputs <- toAssetChangeOutput(assetChange, request.consolidationAddress)
//      boxReferences      <-
//        // do not use arbit boxes if no arbit outputs
//        if (arbitsOwed > 0) toBoxReferencesResult(inputBoxes)
//        else toBoxReferencesResult(inputBoxes.copy(arbits = Set.empty))
//      _ <- validateNonEmptyPolyInputNonces(inputPolyNonces)
//      _ <- validatePositiveOutputValues(polyOutputValues)
//      _ <- validatePositiveOutputValues(arbitOutputValues)
//      _ <- validatePositiveOutputValues(assetOutputValues)
//      _ <- validatePolyFunds(polyFunds, request.fee.data, polysOwed)
//      // only validate the amount of arbit funds when there are some arbits spent in the transfer
//      _ <- arbitFunds.fold(Int128(0).asRight[BuildTransferFailure])(funds => validateArbitFunds(funds, arbitsOwed))
//      _ <-
//        if (!request.minting) validateAssetFunds(assetFunds, assetsOwed)
//        else Map.empty.asRight
//      result <- transfer(boxReferences, assetChangeOutputs)
//    } yield result
//  }

  /**
   * Picks boxes from state using the given context information and selection algorithm.
   * @param addresses the set of address controlling boxes to select from
   * @param polysNeeded the number of polys needed in the transfer
   * @param arbitsNeeded the number of arbits needed in the transfer
   * @param assetsNeeded the number of assets of various asset codes needed in the transfer
   * @param boxAlgorithm the algorithm to use for selecting boxes
   * @param state the box reader to retrieve the existing box state from
   * @return a set of boxes controlled by the given addresses
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
          case (boxes, (addr, box: PolyBox))  => boxes.copy(polys = boxes.polys + (addr -> box))
          case (boxes, (addr, box: ArbitBox)) => boxes.copy(arbits = boxes.arbits + (addr -> box))
          case (boxes, (addr, box: AssetBox)) => boxes.copy(assets = boxes.assets + (addr -> box))
          case (boxes, _)                     => boxes
        }

    BoxSelectionAlgorithm.pickBoxes(boxAlgorithm, boxesFromState, polysNeeded, arbitsNeeded, assetsNeeded)
  }

  /**
   * Picks only poly and arbit boxes from state using the given context information and selection algorithm.
   * @param addresses the set of address controlling boxes to select from
   * @param polysNeeded the number of polys needed in the transfer
   * @param arbitsNeeded the number of arbits needed in the transfer
   * @param boxSelection the algorithm to use for selecting boxes
   * @param state the box reader to retrieve the existing box state from
   * @return a set of boxes controlled by the given addresses
   */
  private def pickPolyAndArbitBoxesFromState(
    addresses:    List[Address],
    polysNeeded:  Int128,
    arbitsNeeded: Int128,
    boxSelection: BoxSelectionAlgorithm,
    state:        BoxReader[ProgramId, Address]
  ): BoxSet =
    pickBoxesFromState(addresses, polysNeeded, arbitsNeeded, Map.empty, boxSelection, state)

  /**
   * Converts the given collection of asset change into a collection of asset outputs.
   * @param assetChange sets of change amounts associated with their asset code
   * @param consolidationAddress the address to send change outputs to
   * @return if successful, a collection of asset change outputs, otherwise a [[BuildTransferFailure]]
   */
  private def toAssetChangeOutput(
    assetChange:          Map[AssetCode, Int128],
    consolidationAddress: FullAddress,
    minting:              Boolean
  ): Either[BuildTransferFailure, List[Transaction.Output]] =
    assetChange.toList
      // attempt to convert each asset into an output
      .traverse[Either[BuildTransferFailure, *], Transaction.Output](asset =>
        asset._1.toTetraAssetCode
          .map(assetCode =>
            Transaction.Output(
              consolidationAddress,
              TetraBox.Values.Asset(asset._2.toSized, assetCode, Sized.strictUnsafe(Bytes.fill(32)(0: Byte)), None),
              minting
            )
          )
          .leftMap {
            case ToTetraAssetCodeFailures.InvalidShortName(shortName) =>
              BuildTransferFailures.InvalidShortName(shortName)
            case ToTetraAssetCodeFailures.InvalidAddress(address) =>
              BuildTransferFailures.InvalidAddress(address)
          }
      )
}
