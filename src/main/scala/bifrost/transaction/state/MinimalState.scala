package bifrost.transaction.state

import bifrost.transaction._
import bifrost.transaction.box.Box
import bifrost.transaction.box.proposition.Proposition
import bifrost.{NodeViewComponent, NodeViewModifier, PersistentNodeViewModifier}
import bifrost.scorexMod.GenericMinimalState
import bifrost.transaction.bifrostTransaction.Transaction

import scala.util.Try

/**
  * Abstract functional interface of state which is a result of a sequential blocks applying
  */

trait MinimalState[P <: Proposition,
BX <: Box[P],
TX <: Transaction[P],
M <: PersistentNodeViewModifier[P, TX],
MS <: MinimalState[P, BX, TX, M, MS]] extends NodeViewComponent {
  self: MS =>
}

object MinimalState {
}