package bifrost.state

/**
  * Created by cykoz on 4/13/17.
  */

import bifrost.modifier.box.GenericBox
import bifrost.modifier.box.proposition.Proposition
import bifrost.modifier.transaction.bifrostTransaction.{GenericTransaction, Transaction}
import bifrost.modifier.ModifierId
import bifrost.nodeView.{NodeViewComponent, NodeViewModifier, PersistentNodeViewModifier}
import bifrost.state.MinimalState.VersionTag

import scala.util.Try

/**
  * Abstract functional interface of state which is a result of a sequential blocks applying
  */

trait MinimalState[T, P <: Proposition,
BX <: GenericBox[P, T],
TX <: GenericTransaction[P],
M <: PersistentNodeViewModifier,
MS <: MinimalState[T, P, BX, TX, M, MS]] extends NodeViewComponent {
  self: MS =>

  def version: VersionTag

  def validate(transaction: TX): Try[Unit]

  def validate(mod: M): Try[Unit] = Try(mod.transactions.getOrElse(Seq()).foreach(tx => validate(tx).get))

  def isValid(tx: TX): Boolean = validate(tx).isSuccess

  def filterValid(txs: Seq[TX]): Seq[TX] = txs.filter(isValid)

  def closedBox(boxId: Array[Byte]): Option[BX]

  //def boxesOf(proposition: P): Seq[BX]

  def changes(mod: M): Try[GenericStateChanges[T, P, BX]]

  def applyChanges(changes: GenericStateChanges[T, P, BX], newVersion: VersionTag): Try[MS]

  def applyModifier(mod: M): Try[MS] = {
    validate(mod) flatMap { r =>
      changes(mod).flatMap(cs => applyChanges(cs, mod.id))
    }
  }

  def applyModifiers(mods: Seq[M]): Try[MS] =
    mods.foldLeft(Try(this)) { case (curTry, mod) =>
      curTry flatMap (_.applyModifier(mod))
    }

  def rollbackTo(version: VersionTag): Try[MS]
}

object MinimalState {
  type VersionTag = ModifierId
}

trait StateFeature

trait TransactionValidation[TX <: Transaction] extends StateFeature {
  def isValid(tx: TX): Boolean = validate(tx).isSuccess

  def filterValid(txs: Seq[TX]): Seq[TX] = txs.filter(isValid)

  def validate(tx: TX): Try[Unit]
}
