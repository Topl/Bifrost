package bifrost.nodeView.state

/**
  * Created by cykoz on 4/13/17.
  */

import bifrost.modifier.ModifierId
import bifrost.nodeView.box.GenericBox
import bifrost.nodeView.box.proposition.Proposition
import bifrost.nodeView.state.MinimalState.VersionTag
import bifrost.nodeView.{ NodeViewComponent, PersistentNodeViewModifier }

import scala.util.Try

/**
  * Abstract functional interface of state which is a result of a sequential blocks applying
  */

trait MinimalState[BX <: GenericBox[_ <: Proposition, _], M <: PersistentNodeViewModifier, MS <: MinimalState[BX, M, MS]]
    extends NodeViewComponent with StateReader[BX]{

  self: MS =>

  def version: VersionTag

  def applyModifier(mod: M): Try[MS]

  def rollbackTo(version: VersionTag): Try[MS]

  def getReader: StateReader[BX] = this
}

object MinimalState {
  type VersionTag = ModifierId
}

