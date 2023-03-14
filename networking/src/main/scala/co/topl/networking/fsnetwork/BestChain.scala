package co.topl.networking.fsnetwork

import cats.data.NonEmptyChain
import cats.implicits.catsSyntaxEq
import co.topl.consensus.models.SlotData
import co.topl.models.TypedIdentifier
import co.topl.models.utility._
import co.topl.typeclasses.implicits._

case class BestChain(slotData: NonEmptyChain[SlotData]) {
  val last: SlotData = slotData.last
  val lastId: TypedIdentifier = last.slotId.blockId: TypedIdentifier

  private lazy val ids: List[TypedIdentifier] =
    slotData.toChain.toList.map(s => blockIdAsTypedBytes(s.slotId.blockId))

  def contains(id: TypedIdentifier): Boolean = ids.contains(id)
  def isLastId(id: TypedIdentifier): Boolean = lastId === id

  def isExtendedBy(extensionChain: NonEmptyChain[SlotData]): Boolean =
    extensionChain.head.parentSlotId == last.slotId || extensionChain.contains(last)
}
