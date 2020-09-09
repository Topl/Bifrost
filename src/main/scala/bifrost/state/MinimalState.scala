package bifrost.state

/**
  * Created by cykoz on 4/13/17.
  */

import bifrost.modifier.ModifierId
import bifrost.modifier.transaction.bifrostTransaction.Transaction
import bifrost.nodeView.{NodeViewComponent, PersistentNodeViewModifier}
import bifrost.state.MinimalState.VersionTag

import scala.util.Try

/**
  * Abstract functional interface of state which is a result of a sequential blocks applying
  */

trait MinimalState[
M <: PersistentNodeViewModifier,
MS <: MinimalState[M, MS]] extends NodeViewComponent with StateReader {
  self: MS =>

  def version: VersionTag

  def applyModifier(mod: M): Try[MS]

  def rollbackTo(version: VersionTag): Try[MS]

  def getReader: StateReader = this
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
