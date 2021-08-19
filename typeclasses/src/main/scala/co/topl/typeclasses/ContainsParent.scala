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

  object Instances {

    implicit val blockContainsParent: ContainsParent[Block] = new ContainsParent[Block] {
      val GenesisBlockIdBytes: Bytes = Bytes(Array.fill[Byte](32)(0))

      override def parentIdOf(t: Block): Option[(Byte, Bytes)] = {
        val rawParentId = t match {
          case b: BlockV1 =>
            b.parentId
          case b: BlockV2 =>
            b.parentId
        }
        Some(rawParentId).filterNot(_.dataBytes == GenesisBlockIdBytes)
      }
    }
  }
}
