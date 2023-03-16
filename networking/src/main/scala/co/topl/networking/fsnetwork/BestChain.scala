package co.topl.networking.fsnetwork

import cats.data.NonEmptyChain
import cats.implicits.catsSyntaxEq
import co.topl.consensus.models.BlockId
import co.topl.consensus.models.SlotData
import co.topl.typeclasses.implicits._

case class BestChain(slotData: NonEmptyChain[SlotData]) {
  val last: SlotData = slotData.last
  val lastId: BlockId = last.slotId.blockId

  private lazy val ids: List[BlockId] =
    slotData.toChain.toList.map(_.slotId.blockId)

  def contains(id: BlockId): Boolean = ids.contains(id)
  def isLastId(id: BlockId): Boolean = lastId === id

  def isExtendedBy(extensionChain: NonEmptyChain[SlotData]): Boolean =
    extensionChain.head.parentSlotId == last.slotId || extensionChain.contains(last)
}
