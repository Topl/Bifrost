package co.topl.typeclasses

import co.topl.models._
import co.topl.typeclasses.Identifiable.ops._
import co.topl.typeclasses.IdentifiableInstances._
import co.topl.typeclasses.Timestamped.ops._
import co.topl.typeclasses.TimestampedInstances._
import simulacrum.{op, typeclass}

@typeclass trait Chainable[T] {
  @op("parentId") def parentId(t:     T): TypedIdentifier
  @op("canChainTo") def canChainTo(t: T, parent: T): Boolean
}

object ChainableInstances {

  implicit val blockChainable: Chainable[Block] =
    new Chainable[Block] {

      override def parentId(t: Block): (Byte, Bytes) = t match {
        case b: BlockV1 => b.parentId
        case b: BlockV2 => b.parentId
      }

      override def canChainTo(t: Block, parent: Block): Boolean =
        BlockValidations.parentIdCheck(t, parent)(this, identifiableBlock) &&
        BlockValidations.timestampCheck(t, parent) &&
        BlockValidations.versionCheck(t, parent)
    }
}

object BlockValidations {
  import Chainable.ops._

  def timestampCheck[T: Timestamped, TParent: Timestamped](t: T, tParent: TParent): Boolean =
    t.timestamp > tParent.timestamp

  def parentIdCheck[T: Chainable, TParent: Identifiable](t: T, tParent: TParent): Boolean =
    t.parentId == tParent.id

  def versionCheck[T <: Block, TParent <: Block](t: T, tParent: TParent): Boolean =
    t match {
      case _: BlockV1 =>
        tParent match {
          case _: BlockV1 => true
          case _: BlockV2 => false
        }
      case _: BlockV2 =>
        true
    }

}
