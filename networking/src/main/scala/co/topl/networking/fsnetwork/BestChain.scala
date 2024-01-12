package co.topl.networking.fsnetwork

import cats.data._
import cats.implicits.catsSyntaxEq
import co.topl.consensus.models.BlockId
import co.topl.consensus.models.SlotData
import co.topl.typeclasses.implicits._

case class BestChain(slotData: NonEmptyChain[SlotData]) {
  val last: SlotData = slotData.last
  val lastId: BlockId = last.slotId.blockId

  def isLastId(id: BlockId): Boolean = lastId === id

  def getNElementId(n: Int): BlockId = slotData.iterator.drop(n).nextOption().getOrElse(slotData.last).slotId.blockId
}
