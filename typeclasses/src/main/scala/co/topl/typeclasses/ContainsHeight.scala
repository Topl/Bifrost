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

  object Instances {

    implicit val blockContainsHeight: ContainsHeight[Block] = {
      case b: BlockV1 => b.height
      case b: BlockV2 => b.height
    }
  }
}
