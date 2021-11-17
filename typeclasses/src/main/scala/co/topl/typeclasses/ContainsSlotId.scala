package co.topl.typeclasses

import co.topl.models.SlotId
import simulacrum._

@typeclass trait ContainsSlotId[T] {
  @op("slotId") def slotIdOf(t: T): SlotId
}

object ContainsSlotId {

  trait Instances {

    implicit def identifiableContainsSlot[T: Identifiable: ContainsSlot]: ContainsSlotId[T] = t =>
      SlotId(ContainsSlot[T].slotOf(t), Identifiable[T].idOf(t))
  }
  object instances extends Instances
}
