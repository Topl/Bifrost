package co.topl.modifier.transaction.builder

import co.topl.attestation.Address
import co.topl.modifier.box._
import co.topl.utils.Int128
import mouse.all._

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
  def pickBoxes(
    algorithm:    BoxSelectionAlgorithm,
    boxes:        BoxMap,
    polysNeeded:  Int128,
    arbitsNeeded: Int128,
    assetsNeeded: Map[AssetCode, Int128]
  ): BoxMap =
    algorithm match {
      case BoxSelectionAlgorithms.All =>
        all(boxes, arbitsNeeded, assetsNeeded)
      case BoxSelectionAlgorithms.SmallestFirst =>
        orderedByValue(boxes, polysNeeded, arbitsNeeded, assetsNeeded, _.quantity)
      case BoxSelectionAlgorithms.LargestFirst =>
        orderedByValue(boxes, polysNeeded, arbitsNeeded, assetsNeeded, -_.quantity)
      case BoxSelectionAlgorithms.Specific(ids) =>
        specific(boxes, ids)
    }

  private def all(from: BoxMap, arbitsNeeded: Int128, assetsNeeded: Map[AssetCode, Int128]): BoxMap =
    from.copy(
      assets = from.assets.filter(box => assetsNeeded.contains(box._2.value.assetCode)),
      arbits = (arbitsNeeded > 0).option(from.arbits).toSet.flatten.toList
    )

  private def orderedByValue(
    from:         BoxMap,
    polysNeeded:  Int128,
    arbitsNeeded: Int128,
    assetsNeeded: Map[AssetCode, Int128],
    orderBy:      TokenValueHolder => Int128
  ): BoxMap =
    BoxMap(
      takeBoxesUntilQuantity[SimpleValue, ArbitBox](
        arbitsNeeded,
        from.arbits.sortBy(box => orderBy(box._2.value))
      ).distinct,
      takeBoxesUntilQuantity[SimpleValue, PolyBox](
        polysNeeded,
        from.polys.sortBy(box => orderBy(box._2.value))
      ).distinct,
      assetsNeeded
        .map { case (assetCode, quantity) =>
          takeBoxesUntilQuantity[AssetValue, AssetBox](
            quantity,
            from.assets
              .filter(_._2.value.assetCode == assetCode)
              .sortBy(box => orderBy(box._2.value))
          )
        }
        .toSet
        .flatten
        .toList
    )

  private def specific(from: BoxMap, ids: List[BoxId]): BoxMap =
    BoxMap(
      from.arbits.filter(box => ids.contains(box._2.id)),
      from.polys.filter(box => ids.contains(box._2.id)),
      from.assets.filter(box => ids.contains(box._2.id))
    )

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
      .foldLeft(Int128(0), List[(Address, T)]()) {
        case ((sum, result), _) if sum >= target => sum                         -> result
        case ((sum, result), box)                => sum + box._2.value.quantity -> (box :: result)
      }
      ._2

}
