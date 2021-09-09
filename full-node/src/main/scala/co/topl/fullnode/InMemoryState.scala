package co.topl.fullnode

import cats.data.NonEmptyChain
import co.topl.models.utility.Ratio
import co.topl.models._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._

case class InMemoryState(
  genesis:        BlockV2,
  blocks:         Map[TypedIdentifier, BlockV2],
  orphans:        Map[TypedIdentifier, BlockV2],
  relativeStakes: Map[Epoch, Map[TaktikosAddress, Ratio]],
  epochNonce:     Map[Epoch, Eta]
) {

  def append(child: BlockV2): InMemoryState =
    block(child.headerV2.parentHeaderId) match {
      case Some(_) => copy(blocks = blocks + (child.headerV2.id -> child))
      case _       => copy(orphans = orphans + (child.headerV2.id -> child))
    }

  def canonicalHead: BlockV2 =
    (Iterator.single(genesis) ++ blocks.valuesIterator).maxBy(_.headerV2.height)

  def block(id: TypedIdentifier): Option[BlockV2] =
    Some(genesis).filter(_.headerV2.id == id).orElse(blocks.get(id)).orElse(orphans.get(id))
}
