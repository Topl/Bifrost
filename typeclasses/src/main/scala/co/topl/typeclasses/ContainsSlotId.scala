package co.topl.typeclasses

import co.topl.codecs.bytes.typeclasses.Identifiable
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.models.SlotId
import simulacrum._
import IdentityOps._

@typeclass trait ContainsSlotId[T] {
  @op("slotId") def slotIdOf(t: T): SlotId
}

object ContainsSlotId {

  trait Instances {

    implicit def identifiableContainsSlot[T: Identifiable: ContainsSlot]: ContainsSlotId[T] = t =>
      SlotId(ContainsSlot[T].slotOf(t), t.id)
  }
  object instances extends Instances
}
