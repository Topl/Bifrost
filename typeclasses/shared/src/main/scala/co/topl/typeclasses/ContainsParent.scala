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
      header => Some(header.parentHeaderId).filterNot(_.dataBytes == BlockGenesis.ParentId)

    implicit val blockV1ContainsParent: ContainsParent[BlockV1] =
      block => Some(block.parentId).filterNot(_.dataBytes == BlockGenesis.ParentId)
  }
  object Instances extends Instances
}
