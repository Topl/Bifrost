package co.topl.modifier.transaction.unsigned

import co.topl.attestation.Address
import co.topl.modifier.BoxReader
import co.topl.modifier.box._
import co.topl.modifier.transaction.unsigned
import co.topl.utils.Int128

object Builder {

  sealed trait BoxPickingStrategy

  object BoxPickingStrategies {
    case object All extends BoxPickingStrategy
    case object SmallestFirst extends BoxPickingStrategy
    case class Specific(ids: List[BoxId]) extends BoxPickingStrategy
  }

  /**
   * Represents a set of available boxes for use in a transaction.
   * @param arbits the available arbit boxes
   * @param polys the available poly boxes
   * @param assets the available asset boxes
   */
  case class TokenBoxes(
    arbits: List[(Address, ArbitBox)],
    polys:  List[(Address, PolyBox)],
    assets: List[(Address, AssetBox)]
  )

  def takeBoxesUntilQuantity[S <: TokenValueHolder, T <: TokenBox[S]](
    x:     Int128,
    boxes: List[(Address, T)]
  ): List[(Address, T)] =
    boxes
      .foldLeft((Int128(0), List[(Address, T)]())) {
        case ((sum, result), _) if sum >= x => sum                         -> result
        case ((sum, result), box)           => sum + box._2.value.quantity -> (result :+ box)
      }
      ._2

  def getAvailableBoxes(senders: List[Address], state: BoxReader[ProgramId, Address]): TokenBoxes =
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

  def pickSmallestBoxesFirst(boxes: TokenBoxes, request: TransferRequest): TokenBoxes =
    request match {
      case TransferRequests.PolyTransferRequest(_, to, _, fee, _, _) =>
        val polyBoxes = takeBoxesUntilQuantity[SimpleValue, PolyBox](
          fee + to.map(_._2).sum,
          boxes.polys.sortBy(_._2.value.quantity)
        )
        TokenBoxes(List(), polyBoxes, List())

      case TransferRequests.AssetTransferRequest(_, _, _, _, fee, _, true, _) =>
        val polyBoxes = takeBoxesUntilQuantity[SimpleValue, PolyBox](fee, boxes.polys.sortBy(_._2.value.quantity))
        TokenBoxes(List(), polyBoxes, List())

      case TransferRequests.AssetTransferRequest(_, to, _, _, fee, _, false, _) =>
        val polyBoxes = takeBoxesUntilQuantity[SimpleValue, PolyBox](fee, boxes.polys.sortBy(_._2.value.quantity))

        val assetBoxes =
          boxes.assets.headOption
            .map(_._2.value.assetCode)
            .map(assetCode =>
              takeBoxesUntilQuantity[AssetValue, AssetBox](
                to.map(_._2.quantity).sum,
                boxes.assets
                  .filter(_._2.value.assetCode == assetCode)
                  .sortBy(_._2.value.quantity)
              )
            )
            .getOrElse(List())

        TokenBoxes(List(), polyBoxes, assetBoxes)

      case TransferRequests.ArbitTransferRequest(_, to, _, _, fee, _, _) =>
        val polyBoxes = takeBoxesUntilQuantity[SimpleValue, PolyBox](fee, boxes.polys.sortBy(_._2.value.quantity))

        val arbitBoxes = takeBoxesUntilQuantity[SimpleValue, ArbitBox](
          to.map(_._2).sum,
          boxes.arbits.sortBy(_._2.value.quantity)
        )

        TokenBoxes(arbitBoxes, polyBoxes, List())
    }

  def pickSpecificBoxes(ids: List[BoxId], boxes: TokenBoxes, request: TransferRequest): TokenBoxes =
    request match {
      case _: TransferRequests.PolyTransferRequest =>
        boxes.copy(polys = boxes.polys.filter(box => ids.contains(box._2.id)))
      case _: TransferRequests.ArbitTransferRequest =>
        boxes.copy(arbits = boxes.arbits.filter(box => ids.contains(box._2.id)))
      case _: TransferRequests.AssetTransferRequest =>
        boxes.copy(assets = boxes.assets.filter(box => ids.contains(box._2.id)))
    }

  def pickBoxes(boxes: TokenBoxes, strategy: BoxPickingStrategy, request: TransferRequest): TokenBoxes =
    strategy match {
      case BoxPickingStrategies.All => boxes
      case BoxPickingStrategies.SmallestFirst =>
        pickSmallestBoxesFirst(boxes, request)
      case BoxPickingStrategies.Specific(ids) =>
        pickSpecificBoxes(ids, boxes, request)
    }

  def buildTransfer(
    boxReader:  BoxReader[ProgramId, Address],
    request:    TransferRequest,
    boxPicking: BoxPickingStrategy
  ): Either[UnsignedTransferFailure, UnsignedTransferTransaction] =
    request match {
      case polysRequest: TransferRequests.PolyTransferRequest =>
        val availableBoxes = getAvailableBoxes(polysRequest.from, boxReader)
        val filteredBoxes = pickBoxes(availableBoxes, boxPicking, polysRequest)

        unsigned.fromPolyTransferRequest(polysRequest, filteredBoxes.polys)

      case arbitsRequest: TransferRequests.ArbitTransferRequest =>
        val availableBoxes = getAvailableBoxes(arbitsRequest.from, boxReader)
        val filteredBoxes = pickBoxes(availableBoxes, boxPicking, arbitsRequest)

        unsigned.fromArbitTransferRequest(arbitsRequest, filteredBoxes.polys, filteredBoxes.arbits)

      case assetsRequest: TransferRequests.AssetTransferRequest =>
        val availableBoxes = getAvailableBoxes(assetsRequest.from, boxReader)
        val filteredBoxes = pickBoxes(availableBoxes, boxPicking, assetsRequest)

        unsigned.fromAssetTransferRequest(assetsRequest, filteredBoxes.polys, filteredBoxes.assets)
    }

}
