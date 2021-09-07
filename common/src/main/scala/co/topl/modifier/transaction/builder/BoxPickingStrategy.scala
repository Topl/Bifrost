package co.topl.modifier.transaction.builder

import co.topl.attestation.Address
import co.topl.modifier.box.{ArbitBox, AssetBox, AssetCode, Box, PolyBox, SimpleValue, TokenBox, TokenValueHolder}
import co.topl.utils.Int128
import simulacrum.typeclass

/**
 * Defines a strategy for choosing which boxes should be used as inputs for a transfer transaction.
 * @tparam T the type of strategy to use to filter the input token boxes
 */
@typeclass
trait BoxPickingStrategy[T] {
  def pick(strategy: T, boxes: TokenBoxes): TokenBoxes
}

object BoxPickingStrategy {

  /**
   * Chooses all token boxes available to addresses sending tokens in a transaction.
   */
  case object All
  type All = All.type

  /**
   * Parameters for choosing the smallest boxes first until a certain limit.
   * @param arbitsUntil choose arbit boxes until this value is reached
   * @param polysUntil choose poly boxes until this value is reached
   * @param assetsUntil choose asset boxes until this value is reached
   */
  case class SmallestFirst(
    arbitsUntil: Option[Int128],
    polysUntil:  Option[Int128],
    assetsUntil: Option[(AssetCode, Int128)]
  )

  /**
   * Parameters for choosing specific boxes defined by a set of box nonces.
   * @param boxNonces the set of nonces to filter boxes by
   */
  case class Specific(boxNonces: IndexedSeq[Box.Nonce])

  trait Instances {

    implicit val allStrategy: BoxPickingStrategy[All.type] = (_, boxes) => boxes

    implicit val smallestStrategy: BoxPickingStrategy[SmallestFirst] = { (strategy, boxes) =>
      def takeBoxesUntil[T <: TokenBox[TokenValueHolder]](
        boxes: IndexedSeq[(Address, T)],
        until: Int128
      ): IndexedSeq[(Address, T)] =
        boxes.sortBy(_._2.value.quantity).foldLeft(IndexedSeq[(Address, T)]()) {
          // take boxes until the sum of all selected boxes is greater-than-or-equal-to the requested amount
          case (selected, _) if selected.map(_._2.value.quantity).sum >= until => selected
          case (selected, box)                                                 => selected :+ box
        }

      val arbitBoxes: IndexedSeq[(Address, ArbitBox)] =
        strategy.arbitsUntil.map(takeBoxesUntil(boxes.arbits, _)).getOrElse(IndexedSeq[(Address, ArbitBox)]())

      val polyBoxes: IndexedSeq[(Address, PolyBox)] =
        strategy.polysUntil.map(takeBoxesUntil(boxes.polys, _)).getOrElse(IndexedSeq[(Address, PolyBox)]())

      val assetBoxes: IndexedSeq[(Address, AssetBox)] =
        strategy.assetsUntil
          .map(takeUntil =>
            // filter out any asset boxes of a different asset code
            takeBoxesUntil(boxes.assets.filter(box => box._2.value.assetCode == takeUntil._1), takeUntil._2)
          )
          .getOrElse(IndexedSeq[(Address, AssetBox)]())

      TokenBoxes(arbitBoxes, polyBoxes, assetBoxes)
    }

    implicit val specificStrategy: BoxPickingStrategy[Specific] = (strategy, boxes) =>
      TokenBoxes(
        boxes.arbits.filter(box => strategy.boxNonces.contains(box._2.nonce)),
        boxes.polys.filter(box => strategy.boxNonces.contains(box._2.nonce)),
        boxes.assets.filter(box => strategy.boxNonces.contains(box._2.nonce))
      )

  }

  trait Implicits extends Instances with ToBoxPickingStrategyOps

  object implicits extends Implicits
}
