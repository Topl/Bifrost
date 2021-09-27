package co.topl.modifier.transaction.builder

import co.topl.attestation.Address
import co.topl.modifier.BoxReader
import co.topl.modifier.box.{
  ArbitBox,
  AssetBox,
  AssetValue,
  BoxId,
  PolyBox,
  ProgramId,
  SimpleValue,
  TokenBox,
  TokenValueHolder
}
import co.topl.utils.Int128
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

sealed trait BoxSelectionAlgorithm

object BoxSelectionAlgorithms {
  case object All extends BoxSelectionAlgorithm
  case object SmallestFirst extends BoxSelectionAlgorithm
  case object LargestFirst extends BoxSelectionAlgorithm
  case class Specific(ids: List[BoxId]) extends BoxSelectionAlgorithm
}

object BoxSelectionAlgorithm {

  /**
   * Takes boxes from the provided list until a certain quantity of funds is reached.
   * @param target the target quantity
   * @param boxes the boxes to take from
   * @tparam S the type of the boxes TokenValueHolder
   * @tparam T the type of the boxes TokenBox
   * @return a set of boxes with a quantity
   */
  private def takeBoxesUntilQuantity[S <: TokenValueHolder, T <: TokenBox[S]](
    target: Int128,
    boxes:  List[(Address, T)]
  ): List[(Address, T)] =
    boxes
      .foldLeft((Int128(0), List[(Address, T)]())) {
        case ((sum, result), _) if sum >= target => sum                         -> result
        case ((sum, result), box)                => sum + box._2.value.quantity -> (result :+ box)
      }
      ._2

  /**
   * Picks from the list of available unopened boxes in a sorted order starting from the front of the list until
   * it hits the quantity requested by the input request.
   * @param boxes the list of available boxes
   * @param request the transfer request
   * @param sortQuantity the method used for sorting the inputs
   * @return a set of boxes to use as inputs to a transaction
   */
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
          // get assets matching asset code
          to.map(_._2.assetCode)
            .headOption
            .map(assetCode => boxes.assets.filter(_._2.value.assetCode == assetCode))
            // get the first assets sorted that sum to or beyond the recipient quantity
            .map(assets =>
              takeBoxesUntilQuantity[AssetValue, AssetBox](
                assets.map(_._2.value.quantity).sum,
                assets.sortBy(box => sortQuantity(box._2.value))
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

  /**
   * Picks any boxes with an ID contained in the provided list.
   * @param ids the list of IDs to filter boxes by
   * @param boxes the set of available boxes
   * @param request the transfer request to pick boxes for
   * @return a set of boxes to use in a transfer transaction
   */
  private def pickSpecificBoxes(ids: List[BoxId], boxes: TokenBoxes, request: TransferRequest): TokenBoxes =
    request match {
      case _: TransferRequests.PolyTransferRequest =>
        boxes.copy(polys = boxes.polys.filter(box => ids.contains(box._2.id)))
      case _: TransferRequests.ArbitTransferRequest =>
        boxes.copy(
          arbits = boxes.arbits.filter(box => ids.contains(box._2.id)),
          polys = boxes.polys.filter(box => ids.contains(box._2.id))
        )
      case _: TransferRequests.AssetTransferRequest =>
        boxes.copy(
          assets = boxes.assets.filter(box => ids.contains(box._2.id)),
          polys = boxes.polys.filter(box => ids.contains(box._2.id))
        )
    }

  private def pickAllBoxes(boxes: TokenBoxes, request: TransferRequest): TokenBoxes =
    request match {
      case TransferRequests.AssetTransferRequest(_, to, _, _, _, _, _) =>
        // only pick boxes matching with matching asset codes
        to.map(_._2.assetCode)
          .headOption
          .map(assetCode => boxes.copy(assets = boxes.assets.filter(_._2.value.assetCode == assetCode)))
          .getOrElse(boxes.copy(assets = List()))
      case _ => boxes
    }

  /**
   * Picks boxes to use in a transfer transaction based on the type of algorithm provided.
   * @param boxes the available boxes to spend in a transaction
   * @param algorithm the selection algorithm to apply to the boxes
   * @param request the request for the transfer that the boxes will be used in
   * @return a set of token boxes that should be used for a transfer transaction
   */
  def pickBoxes(algorithm: BoxSelectionAlgorithm, boxes: TokenBoxes, request: TransferRequest): TokenBoxes =
    algorithm match {
      case BoxSelectionAlgorithms.All =>
        pickAllBoxes(boxes, request)
      case BoxSelectionAlgorithms.SmallestFirst =>
        // _.quantity will sort boxes from smallest to largest
        pickBoxesWithSorting(boxes, request, _.quantity)
      case BoxSelectionAlgorithms.LargestFirst =>
        // -_.quantity will sort boxes from largest to smallest
        pickBoxesWithSorting(boxes, request, -_.quantity)
      case BoxSelectionAlgorithms.Specific(ids) =>
        pickSpecificBoxes(ids, boxes, request)
    }

  val jsonDecoder: Decoder[BoxSelectionAlgorithm] = deriveDecoder
  val jsonEncoder: Encoder[BoxSelectionAlgorithm] = deriveEncoder
}
