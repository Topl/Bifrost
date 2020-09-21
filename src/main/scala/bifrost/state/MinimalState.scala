package bifrost.state

/**
  * Created by cykoz on 4/13/17.
  */

import bifrost.modifier.ModifierId
import bifrost.modifier.box.GenericBox
import bifrost.modifier.box.proposition.Proposition
import bifrost.nodeView.{ NodeViewComponent, PersistentNodeViewModifier }
import bifrost.state.MinimalState.VersionTag

import scala.util.Try

/**
  * Abstract functional interface of state which is a result of a sequential blocks applying
  */

trait MinimalState[T, P <: Proposition, BX <: GenericBox[P, T], M <: PersistentNodeViewModifier, MS <: MinimalState[T, P, BX, M, MS]]
    extends NodeViewComponent with StateReader[BX, P, T]{

  self: MS =>

  def version: VersionTag

  def applyModifier(mod: M): Try[MS]

  def rollbackTo(version: VersionTag): Try[MS]

  def getReader: StateReader[BX, P, T] = this
}

object MinimalState {
  type VersionTag = ModifierId
}

