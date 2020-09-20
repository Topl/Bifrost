package bifrost.state

/**
  * Created by cykoz on 4/13/17.
  */

import bifrost.modifier.ModifierId
import bifrost.nodeView.{ NodeViewComponent, PersistentNodeViewModifier }
import bifrost.state.MinimalState.VersionTag

import scala.util.Try

/**
  * Abstract functional interface of state which is a result of a sequential blocks applying
  */

trait MinimalState[M <: PersistentNodeViewModifier, MS <: MinimalState[M, MS]]
    extends NodeViewComponent {
  self: MS =>

  def version: VersionTag

  def applyModifier(mod: M): Try[MS]

  def rollbackTo(version: VersionTag): Try[MS]
}

object MinimalState {
  type VersionTag = ModifierId
}

