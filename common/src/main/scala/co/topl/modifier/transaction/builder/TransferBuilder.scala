package co.topl.modifier.transaction.builder

import cats.implicits._
import co.topl.attestation.{Address, EvidenceProducer, Proposition}
import co.topl.modifier.box._
import co.topl.modifier.transaction.builder.BoxCache.BoxSet
import co.topl.modifier.transaction.builder.Validation._
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer}
import co.topl.modifier.{BoxReader, ProgramId}
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

    // filter the available boxes as specified in the box selection algorithm
    val filteredBoxes = BoxSelectionAlgorithm.pickBoxes(boxSelection, availableBoxes, request)
    val inputPolys = filteredBoxes.polys

    for {
      _                      <- validateNonEmptyInputs[SimpleValue, PolyBox](inputPolys)
      _                      <- validateUniqueInputs[SimpleValue, PolyBox](inputPolys)
      _                      <- validateNonEmptyRecipients(request.to)
      _                      <- validateUniqueRecipients(request.to)
      polysAvailableAfterFee <- validateFeeFunds(boxFunds(inputPolys), request.fee)
      amountOwed = request.to.map(_._2).sum
      changeAmount <- validatePaymentFunds(polysAvailableAfterFee, amountOwed)
      changeRecipient = request.changeAddress -> SimpleValue(changeAmount)
      recipientValues = request.to.map(x => x._1 -> SimpleValue(x._2))
      fromBoxNonces = inputPolys.map(box => box._1 -> box._2.nonce)
    } yield PolyTransfer[P](
      fromBoxNonces.toIndexedSeq,
      (changeRecipient +: recipientValues).toIndexedSeq,
      ListMap(),
      request.fee,
      Instant.now.toEpochMilli,
      request.data,
      false
    )
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

    // filter the available boxes as specified in the box selection algorithm
    val filteredBoxes = BoxSelectionAlgorithm.pickBoxes(boxSelection, availableBoxes, request)
    val inputPolys = filteredBoxes.polys
    val inputAssets = filteredBoxes.assets

    for {
      _ <- validateNonEmptyInputs[SimpleValue, PolyBox](inputPolys)
      _ <- validateUniqueInputs[SimpleValue, PolyBox](inputPolys)
      _ <- validateAssetInputs(inputAssets, request.minting)
      _ <- validateNonEmptyRecipients(request.to)
      _ <- validateUniqueRecipients(request.to)
      assetCode = request.to.head._2.assetCode
      _          <- validateSameAssetCode(assetCode, filteredBoxes.assets, request.minting)
      polyChange <- validateFeeFunds(boxFunds(filteredBoxes.polys), request.fee)
      assetPayment = request.to.map(_._2.quantity).sum
      assetChange <-
        // no input assets required when minting
        if (!request.minting) validatePaymentFunds(boxFunds(filteredBoxes.assets), assetPayment)
        else boxFunds(filteredBoxes.assets).asRight
      assetInputNonces =
        // do not use any inputs when minting
        if (!request.minting) inputAssets.map(box => box._1 -> box._2.nonce)
        else List()
      polyInputNonces = inputPolys.map(box => box._1 -> box._2.nonce)
      changeOutput = request.changeAddress             -> SimpleValue(polyChange)
      assetChangeOutput = request.consolidationAddress -> AssetValue(assetChange, assetCode)
    } yield AssetTransfer[P](
      (polyInputNonces ++ assetInputNonces).toIndexedSeq,
      IndexedSeq(changeOutput, assetChangeOutput) ++ request.to,
      ListMap(),
      request.fee,
      Instant.now.toEpochMilli,
      request.data,
      request.minting
    )
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

    // filter the available boxes as specified in the box selection algorithm
    val filteredBoxes = BoxSelectionAlgorithm.pickBoxes(boxSelection, availableBoxes, request)
    val inputPolys = filteredBoxes.polys
    val inputArbits = filteredBoxes.arbits

    for {
      _      <- validateNonEmptyInputs[SimpleValue, PolyBox](inputPolys)
      _      <- validateUniqueInputs[SimpleValue, PolyBox](inputPolys)
      _      <- validateNonEmptyInputs[SimpleValue, ArbitBox](inputArbits)
      _      <- validateUniqueInputs[SimpleValue, ArbitBox](inputArbits)
      _      <- validateNonEmptyRecipients(request.to)
      _      <- validateUniqueRecipients(request.to)
      change <- validateFeeFunds(inputPolys.map(_._2.value.quantity).sum, request.fee)
      arbitsAvailable = inputArbits.map(_._2.value.quantity).sum
      arbitsOwed = request.to.map(_._2).sum
      arbitChange <- validatePaymentFunds(arbitsAvailable, arbitsOwed)
      changeOutput = request.changeAddress             -> SimpleValue(change)
      arbitChangeOutput = request.consolidationAddress -> SimpleValue(arbitChange)
      arbitOutputs = request.to.map(x => x._1 -> SimpleValue(x._2))
      inputBoxNonces = (inputPolys ++ inputArbits).map(x => x._1 -> x._2.nonce)
    } yield ArbitTransfer[P](
      inputBoxNonces.toIndexedSeq,
      IndexedSeq(changeOutput, arbitChangeOutput) ++ arbitOutputs,
      ListMap(),
      request.fee,
      Instant.now.toEpochMilli,
      request.data,
      false
    )
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
}
