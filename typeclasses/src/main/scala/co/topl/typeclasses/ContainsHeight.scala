package co.topl.typeclasses

import co.topl.models._
import simulacrum.{op, typeclass}

/**
 * Satisfies that T contains some absolute height
 */
@typeclass trait ContainsHeight[T] {
  @op("height") def heightOf(t: T): Long
}

object ContainsHeight {

  trait Instances {
    implicit val blockHeaderV2: ContainsHeight[BlockHeaderV2] = _.height
    implicit val blockV1: ContainsHeight[BlockV1] = _.height
  }
  object Instances extends Instances
}
