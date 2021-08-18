package co.topl.typeclasses

import co.topl.models._
import co.topl.typeclasses.ContainsParent.Instances._
import co.topl.typeclasses.ContainsParent.ops._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._
import co.topl.typeclasses.ContainsTimestamp.Instances._
import co.topl.typeclasses.ContainsTimestamp.ops._
import simulacrum.{op, typeclass}

/**
 * Satisfies that T may or may not be a child of some other T
 */
@typeclass trait ChainableValidation[T] {
  @op("isChildOf") def validateChildAssociation(t: T, parent: T): Boolean
}

object ChainableValidation {

  object Instances {

    implicit val blockChainable: ChainableValidation[Block] =
      new ChainableValidation[Block] {
        implicit private def c: ChainableValidation[Block] = this

        override def validateChildAssociation(t: Block, parent: Block): Boolean =
          BlockValidations.parentIdCheck(t, parent) &&
          BlockValidations.timestampCheck(t, parent) &&
          BlockValidations.versionCheck(t, parent)
      }
  }
}

object BlockValidations {

  def timestampCheck[T: ContainsTimestamp, TParent: ContainsTimestamp](t: T, tParent: TParent): Boolean =
    t.timestamp > tParent.timestamp

  def parentIdCheck[T: ChainableValidation: ContainsParent, TParent: Identifiable](t: T, tParent: TParent): Boolean =
    t.parentId.contains(tParent.id)

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

/**
 * A typeclass that produces the genesis item for an empty chain of `T`
 */
@typeclass trait ChainGenesis[T] {
  def apply(): T
}
