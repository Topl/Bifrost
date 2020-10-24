package co.topl.nodeView.state

/**
  * Created by cykoz on 4/13/17.
  */

import co.topl.modifier.ModifierId
import co.topl.modifier.block.PersistentNodeViewModifier
import co.topl.nodeView.NodeViewComponent
import co.topl.nodeView.state.MinimalState.VersionTag
import co.topl.nodeView.state.box.GenericBox
import co.topl.crypto.proposition.Proposition

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

