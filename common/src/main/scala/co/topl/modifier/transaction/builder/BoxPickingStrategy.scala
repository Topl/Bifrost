package co.topl.modifier.transaction.builder

import co.topl.attestation.Address
import co.topl.modifier.box.{ArbitBox, AssetBox, Box, PolyBox}
import co.topl.utils.Int128
import simulacrum.typeclass

@typeclass
trait BoxPickingStrategy[T] {
  def pick(strategy: T, boxes: TokenBoxes): TokenBoxes
}

object BoxPickingStrategy {

  case object All
  type All = All.type

  case class SmallestFirst(arbitsUntil: Int128, polysUntil: Int128, assetsUntil: Int128)

  case class Specific(boxNonces: IndexedSeq[Box.Nonce])

  trait Instances {
    implicit val allStrategy: BoxPickingStrategy[All.type] = (_, boxes) => boxes

    implicit val smallestStrategy: BoxPickingStrategy[SmallestFirst] = { (strategy, boxes) =>
      val arbitBoxes = boxes.arbits.sortBy(_._2.value.quantity).foldLeft(IndexedSeq[(Address, ArbitBox)]()) {
        case (selected, _) if selected.map(_._2.value.quantity).sum >= strategy.arbitsUntil => selected
        case (selected, box)                                                                => selected :+ box
      }

      val polyBoxes = boxes.polys.sortBy(_._2.value.quantity).foldLeft(IndexedSeq[(Address, PolyBox)]()) {
        case (selected, _) if selected.map(_._2.value.quantity).sum >= strategy.polysUntil => selected
        case (selected, box)                                                               => selected :+ box
      }

      val assetBoxes = boxes.assets.sortBy(_._2.value.quantity).foldLeft(IndexedSeq[(Address, AssetBox)]()) {
        case (selected, _) if selected.map(_._2.value.quantity).sum >= strategy.assetsUntil => selected
        case (selected, box)                                                                => selected :+ box
      }

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
