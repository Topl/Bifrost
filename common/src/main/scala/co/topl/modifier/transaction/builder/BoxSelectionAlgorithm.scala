package co.topl.modifier.transaction.builder

import cats.data.Chain
import co.topl.attestation.Address
import co.topl.modifier.box._
import co.topl.modifier.transaction.builder.BoxCache.BoxSet
import co.topl.utils.Int128
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}

sealed trait BoxSelectionAlgorithm

object BoxSelectionAlgorithms {
  case object All extends BoxSelectionAlgorithm
  type All = All.type
  case object SmallestFirst extends BoxSelectionAlgorithm
  type SmallestFirst = SmallestFirst.type
  case object LargestFirst extends BoxSelectionAlgorithm
  type LargestFirst = LargestFirst.type
  case class Specific(ids: List[BoxId]) extends BoxSelectionAlgorithm
}

object BoxSelectionAlgorithm {

  /**
   * Picks boxes to use in a transfer transaction based on the type of algorithm provided.
   * @param boxes the available boxes to spend in a transaction
   * @param algorithm the selection algorithm to apply to the boxes
   * @param request the request for the transfer that the boxes will be used in
   * @return a set of token boxes that should be used for a transfer transaction
   */
  def pickBoxes(algorithm: BoxSelectionAlgorithm, boxes: BoxSet, request: TransferRequest): BoxSet =
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

  def pickBoxes(
    algorithm:    BoxSelectionAlgorithm,
    available:    BoxSet,
    polysNeeded:  Int128,
    arbitsNeeded: Int128,
    assetsNeeded: Chain[(AssetCode, Int128)]
  ): BoxSet =
    algorithm match {
      case BoxSelectionAlgorithms.All => pickAllBoxes(available, assetsNeeded)
      case BoxSelectionAlgorithms.SmallestFirst =>
        pickBoxesWithSorting(available, polysNeeded, arbitsNeeded, assetsNeeded, _.quantity)
      case BoxSelectionAlgorithms.LargestFirst =>
        pickBoxesWithSorting(available, polysNeeded, arbitsNeeded, assetsNeeded, -_.quantity)
      case BoxSelectionAlgorithms.Specific(ids) =>
        pickSpecificBoxes(ids, available)
    }

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
    boxes:        BoxSet,
    request:      TransferRequest,
    sortQuantity: TokenValueHolder => Int128
  ): BoxSet =
    request match {
      case TransferRequests.PolyTransferRequest(_, to, _, fee, _) =>
        val polyBoxes = takeBoxesUntilQuantity[SimpleValue, PolyBox](
          fee + to.map(_._2).sum,
          boxes.polys.sortBy(box => sortQuantity(box._2.value))
        )
        BoxSet(List(), polyBoxes, List())

      case TransferRequests.AssetTransferRequest(_, _, _, _, fee, _, true) =>
        val polyBoxes =
          takeBoxesUntilQuantity[SimpleValue, PolyBox](fee, boxes.polys.sortBy(box => sortQuantity(box._2.value)))
        BoxSet(List(), polyBoxes, List())

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

        BoxSet(List(), polyBoxes, assetBoxes)

      case TransferRequests.ArbitTransferRequest(_, to, _, _, fee, _) =>
        val polyBoxes =
          takeBoxesUntilQuantity[SimpleValue, PolyBox](fee, boxes.polys.sortBy(box => sortQuantity(box._2.value)))

        val arbitBoxes = takeBoxesUntilQuantity[SimpleValue, ArbitBox](
          to.map(_._2).sum,
          boxes.arbits.sortBy(box => sortQuantity(box._2.value))
        )

        BoxSet(arbitBoxes, polyBoxes, List())
    }

  private def pickBoxesWithSorting(
    available:    BoxSet,
    polysNeeded:  Int128,
    arbitsNeeded: Int128,
    assetsNeeded: Chain[(AssetCode, Int128)],
    sortBy:       TokenValueHolder => Int128
  ): BoxSet =
    // get boxes of each type that sum to or exceed the requested quantity
    BoxSet(
      takeBoxesUntilQuantity[SimpleValue, ArbitBox](arbitsNeeded, available.arbits.sortBy(box => sortBy(box._2.value))),
      takeBoxesUntilQuantity[SimpleValue, PolyBox](polysNeeded, available.polys.sortBy(box => sortBy(box._2.value))),
      assetsNeeded
        .map { case (assetCode, quantity) =>
          takeBoxesUntilQuantity[AssetValue, AssetBox](
            quantity,
            available.assets.filter(_._2.value.assetCode == assetCode).sortBy(box => sortBy(box._2.value))
          )
        }
        .toList
        .flatten
    )

  /**
   * Picks any boxes with an ID contained in the provided list.
   * @param ids the list of IDs to filter boxes by
   * @param boxes the set of available boxes
   * @param request the transfer request to pick boxes for
   * @return a set of boxes to use in a transfer transaction
   */
  private def pickSpecificBoxes(ids: List[BoxId], boxes: BoxSet, request: TransferRequest): BoxSet =
    request match {
      case _: TransferRequests.PolyTransferRequest =>
        boxes.copy(polys = boxes.polys.filter(box => ids.contains(box._2.id)))
      case _: TransferRequests.ArbitTransferRequest =>
        boxes.copy(
          arbits = boxes.arbits.filter(box => ids.contains(box._2.id)),
          polys = boxes.polys.filter(box => ids.contains(box._2.id))
        )
      case transfer: TransferRequests.AssetTransferRequest =>
        boxes.copy(
          assets = boxes.assets.filter(box => ids.contains(box._2.id)),
          polys = boxes.polys.filter(box => ids.contains(box._2.id))
        )
    }

  private def pickSpecificBoxes(
    ids:   List[BoxId],
    boxes: BoxSet
  ): BoxSet =
    // filter boxes of each type by matching IDs
    BoxSet(
      boxes.arbits.filter(box => ids.contains(box._2.id)),
      boxes.polys.filter(box => ids.contains(box._2.id)),
      boxes.assets.filter(box => ids.contains(box._2.id))
    )

  private def pickAllBoxes(boxes: BoxSet, request: TransferRequest): BoxSet =
    request match {
      case TransferRequests.AssetTransferRequest(_, to, _, _, _, _, _) =>
        // only pick boxes matching with matching asset codes
        to.map(_._2.assetCode)
          .headOption
          .map(assetCode => boxes.copy(assets = boxes.assets.filter(_._2.value.assetCode == assetCode)))
          .getOrElse(boxes.copy(assets = List()))
      case _ => boxes
    }

  private def pickAllBoxes(
    boxes:        BoxSet,
    assetsNeeded: Chain[(AssetCode, Int128)]
  ): BoxSet =
    // only pick boxes with asset codes matching needed assets
    boxes.copy(assets = boxes.assets.filter(box => assetsNeeded.exists(_._1 == box._2.value.assetCode)))
}
