package co.topl.typeclasses

import co.topl.models._
import simulacrum.{op, typeclass}

/**
 * Satisfies that T contains an optional reference to some parent
 */
@typeclass trait ContainsParent[T] {
  @op("parentId") def parentIdOf(t: T): Option[TypedIdentifier]
}

object ContainsParent {

  trait Instances {

    implicit val blockHeaderV2ContainsParent: ContainsParent[BlockHeaderV2] =
      header => if (header.parentSlot >= 0) Some(header.parentHeaderId) else None

    implicit val blockV1ContainsParent: ContainsParent[BlockV1] =
      block => if (block.height > 1) Some(block.parentId) else None
  }
  object Instances extends Instances
}
