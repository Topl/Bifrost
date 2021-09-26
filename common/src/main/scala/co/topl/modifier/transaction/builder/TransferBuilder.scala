package co.topl.modifier.transaction.builder

import cats.implicits._
import co.topl.attestation.{Address, EvidenceProducer, Proposition}
import co.topl.modifier.BoxReader
import co.topl.modifier.box._
import co.topl.modifier.transaction.builder.Validation._
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer}
import co.topl.utils.{Identifiable, Int128}

import java.time.Instant
import scala.collection.immutable.ListMap

object TransferBuilder {

  /**
   * Represents a set of available boxes for use in a transaction.
   * @param arbits the available arbit boxes
   * @param polys the available poly boxes
   * @param assets the available asset boxes
   */
  private case class TokenBoxes(
    arbits: List[(Address, ArbitBox)],
    polys:  List[(Address, PolyBox)],
    assets: List[(Address, AssetBox)]
  )

  private def takeBoxesUntilQuantity[S <: TokenValueHolder, T <: TokenBox[S]](
    x:     Int128,
    boxes: List[(Address, T)]
  ): List[(Address, T)] =
    boxes
      .foldLeft((Int128(0), List[(Address, T)]())) {
        case ((sum, result), _) if sum >= x => sum                         -> result
        case ((sum, result), box)           => sum + box._2.value.quantity -> (result :+ box)
      }
      ._2

  private def getAvailableBoxes(senders: List[Address], state: BoxReader[ProgramId, Address]): TokenBoxes =
    senders
      .flatMap(addr =>
        state
          .getTokenBoxes(addr)
          .getOrElse(List())
          .map(addr -> _)
      )
      .foldLeft(TokenBoxes(List(), List(), List())) {
        case (boxes, (addr, box: PolyBox))  => boxes.copy(polys = boxes.polys :+ (addr -> box))
        case (boxes, (addr, box: ArbitBox)) => boxes.copy(arbits = boxes.arbits :+ (addr -> box))
        case (boxes, (addr, box: AssetBox)) => boxes.copy(assets = boxes.assets :+ (addr -> box))
        case (boxes, _)                     => boxes
      }

  private def pickBoxesWithSorting(
    boxes:        TokenBoxes,
    request:      TransferRequest,
    sortQuantity: TokenValueHolder => Int128
  ): TokenBoxes =
    request match {
      case TransferRequests.PolyTransferRequest(_, to, _, fee, _) =>
        val polyBoxes = takeBoxesUntilQuantity[SimpleValue, PolyBox](
          fee + to.map(_._2).sum,
          boxes.polys.sortBy(box => sortQuantity(box._2.value))
        )
        TokenBoxes(List(), polyBoxes, List())

      case TransferRequests.AssetTransferRequest(_, _, _, _, fee, _, true) =>
        val polyBoxes =
          takeBoxesUntilQuantity[SimpleValue, PolyBox](fee, boxes.polys.sortBy(box => sortQuantity(box._2.value)))
        TokenBoxes(List(), polyBoxes, List())

      case TransferRequests.AssetTransferRequest(_, to, _, _, fee, _, false) =>
        val polyBoxes =
          takeBoxesUntilQuantity[SimpleValue, PolyBox](fee, boxes.polys.sortBy(box => sortQuantity(box._2.value)))

        val assetBoxes =
          boxes.assets.headOption
            .map(_._2.value.assetCode)
            .map(assetCode =>
              takeBoxesUntilQuantity[AssetValue, AssetBox](
                to.map(_._2.quantity).sum,
                boxes.assets
                  .filter(_._2.value.assetCode == assetCode)
                  .sortBy(box => sortQuantity(box._2.value))
              )
            )
            .getOrElse(List())

        TokenBoxes(List(), polyBoxes, assetBoxes)

      case TransferRequests.ArbitTransferRequest(_, to, _, _, fee, _) =>
        val polyBoxes =
          takeBoxesUntilQuantity[SimpleValue, PolyBox](fee, boxes.polys.sortBy(box => sortQuantity(box._2.value)))

        val arbitBoxes = takeBoxesUntilQuantity[SimpleValue, ArbitBox](
          to.map(_._2).sum,
          boxes.arbits.sortBy(box => sortQuantity(box._2.value))
        )

        TokenBoxes(arbitBoxes, polyBoxes, List())
    }

  private def pickSpecificBoxes(ids: List[BoxId], boxes: TokenBoxes, request: TransferRequest): TokenBoxes =
    request match {
      case _: TransferRequests.PolyTransferRequest =>
        boxes.copy(polys = boxes.polys.filter(box => ids.contains(box._2.id)))
      case _: TransferRequests.ArbitTransferRequest =>
        boxes.copy(arbits = boxes.arbits.filter(box => ids.contains(box._2.id)))
      case _: TransferRequests.AssetTransferRequest =>
        boxes.copy(assets = boxes.assets.filter(box => ids.contains(box._2.id)))
    }

  private def pickBoxes(boxes: TokenBoxes, algorithm: BoxSelectionAlgorithm, request: TransferRequest): TokenBoxes =
    algorithm match {
      case BoxSelectionAlgorithms.All => boxes
      case BoxSelectionAlgorithms.SmallestFirst =>
        pickBoxesWithSorting(boxes, request, _.quantity)
      case BoxSelectionAlgorithms.LargestFirst =>
        // sort box quantity by negative value which results in largest first
        pickBoxesWithSorting(boxes, request, -_.quantity)
      case BoxSelectionAlgorithms.Specific(ids) =>
        pickSpecificBoxes(ids, boxes, request)
    }

  private def boxFunds(fromBoxes: List[(Address, Box[TokenValueHolder])]): Int128 =
    fromBoxes.map(_._2.value.quantity).sum

  def buildUnsignedPolyTransfer[P <: Proposition: Identifiable: EvidenceProducer](
    boxReader:    BoxReader[ProgramId, Address],
    request:      TransferRequests.PolyTransferRequest,
    boxSelection: BoxSelectionAlgorithm
  ): Either[BuildTransferFailure, PolyTransfer[P]] = {
    val availableBoxes = getAvailableBoxes(request.from, boxReader)
    val filteredBoxes = pickBoxes(availableBoxes, boxSelection, request)

    for {
      _             <- validateNonEmptyInputs[SimpleValue, PolyBox](filteredBoxes.polys)
      _             <- validateUniqueInputs[SimpleValue, PolyBox](filteredBoxes.polys)
      _             <- validateNonEmptyRecipients(request.to)
      _             <- validateUniqueRecipients(request.to)
      amountToSpend <- validateFeeFunds(boxFunds(filteredBoxes.polys), request.fee)
      paymentAmount = request.to.map(_._2).sum
      changeAmount <- validatePaymentFunds(amountToSpend, paymentAmount)
      changeRecipient = request.changeAddress -> SimpleValue(changeAmount)
      recipientValues = request.to.map(x => x._1 -> SimpleValue(x._2))
      nonZeroRecipients = (changeRecipient +: recipientValues).filter(x => x._2.quantity > 0)
      fromBoxNonces = filteredBoxes.polys.map(box => box._1 -> box._2.nonce)
    } yield PolyTransfer[P](
      fromBoxNonces.toIndexedSeq,
      nonZeroRecipients.toIndexedSeq,
      ListMap(),
      request.fee,
      Instant.now.toEpochMilli,
      request.data,
      false
    )
  }

  def buildUnsignedAssetTransfer[P <: Proposition: Identifiable: EvidenceProducer](
    boxReader:    BoxReader[ProgramId, Address],
    request:      TransferRequests.AssetTransferRequest,
    boxSelection: BoxSelectionAlgorithm
  ): Either[BuildTransferFailure, AssetTransfer[P]] = {
    val availableBoxes = getAvailableBoxes(request.from, boxReader)
    val filteredBoxes = pickBoxes(availableBoxes, boxSelection, request)

    for {
      _ <- validateNonEmptyInputs[SimpleValue, PolyBox](filteredBoxes.polys)
      _ <- validateUniqueInputs[SimpleValue, PolyBox](filteredBoxes.polys)
      _ <- validateAssetInputs(filteredBoxes.assets, request.minting)
      _ <- validateNonEmptyRecipients(request.to)
      _ <- validateUniqueRecipients(request.to)
      assetCode = request.to.head._2.assetCode
      _          <- validateSameAssetCode(assetCode, filteredBoxes.assets, request.minting)
      polyChange <- validateFeeFunds(boxFunds(filteredBoxes.polys), request.fee)
      assetPayment = request.to.map(_._2.quantity).sum
      assetChange <-
        if (!request.minting) validatePaymentFunds(boxFunds(filteredBoxes.assets), assetPayment)
        else Int128(0).asRight
      assetBoxNonces = filteredBoxes.assets.map(box => box._1 -> box._2.nonce)
      polyBoxNonces = filteredBoxes.polys.map(box => box._1 -> box._2.nonce)
      changeOutput = request.changeAddress       -> SimpleValue(polyChange)
      assetOutput = request.consolidationAddress -> AssetValue(assetChange, assetCode)
      nonZeroOutputs = (changeOutput +: assetOutput +: request.to).filter(x => x._2.quantity > 0)
    } yield AssetTransfer[P](
      (polyBoxNonces ++ assetBoxNonces).toIndexedSeq,
      nonZeroOutputs.toIndexedSeq,
      ListMap(),
      request.fee,
      Instant.now.toEpochMilli,
      request.data,
      request.minting
    )
  }

  def buildUnsignedArbitTransfer[P <: Proposition: Identifiable: EvidenceProducer](
    boxReader:    BoxReader[ProgramId, Address],
    request:      TransferRequests.ArbitTransferRequest,
    boxSelection: BoxSelectionAlgorithm
  ): Either[BuildTransferFailure, ArbitTransfer[P]] = {
    val availableBoxes = getAvailableBoxes(request.from, boxReader)
    val filteredBoxes = pickBoxes(availableBoxes, boxSelection, request)

    for {
      _      <- validateNonEmptyInputs[SimpleValue, PolyBox](filteredBoxes.polys)
      _      <- validateUniqueInputs[SimpleValue, PolyBox](filteredBoxes.polys)
      _      <- validateNonEmptyInputs[SimpleValue, ArbitBox](filteredBoxes.arbits)
      _      <- validateUniqueInputs[SimpleValue, ArbitBox](filteredBoxes.arbits)
      _      <- validateNonEmptyRecipients(request.to)
      _      <- validateUniqueRecipients(request.to)
      change <- validateFeeFunds(filteredBoxes.polys.map(_._2.value.quantity).sum, request.fee)
      arbitsAvailable = filteredBoxes.arbits.map(_._2.value.quantity).sum
      arbitsToSend = request.to.map(_._2).sum
      arbitChange <- validatePaymentFunds(arbitsAvailable, arbitsToSend)
      changeOutput = request.changeAddress             -> SimpleValue(change)
      arbitChangeOutput = request.consolidationAddress -> SimpleValue(arbitChange)
      arbitOutputs = request.to.map(x => x._1 -> SimpleValue(x._2))
      nonZeroOutputs = (changeOutput +: arbitChangeOutput +: arbitOutputs).filter(x => x._2.quantity > 0)
      inputBoxNonces = (filteredBoxes.polys ++ filteredBoxes.arbits).map(x => x._1 -> x._2.nonce)
    } yield ArbitTransfer[P](
      inputBoxNonces.toIndexedSeq,
      nonZeroOutputs.toIndexedSeq,
      ListMap(),
      request.fee,
      Instant.now.toEpochMilli,
      request.data,
      false
    )
  }
}
