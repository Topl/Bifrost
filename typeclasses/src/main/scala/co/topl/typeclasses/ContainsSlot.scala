package co.topl.typeclasses

import co.topl.models.{BlockHeaderV2, Slot}
import simulacrum.{op, typeclass}

/**
 * Satisfies that T contains some absolute height
 */
@typeclass trait ContainsSlot[T] {
  @op("slot") def slotOf(t: T): Slot
}

object ContainsSlot {

  trait Instances {
    implicit val blockHeaderV2ContainsSlot: ContainsSlot[BlockHeaderV2] = _.slot
  }
  object instances extends Instances
}
