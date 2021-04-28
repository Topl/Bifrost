package co.topl.nodeView.state

/**
 * Created by cykoz on 4/13/17.
 */

import co.topl.attestation.Address
import co.topl.modifier.ModifierId
import co.topl.modifier.block.PersistentNodeViewModifier
import co.topl.modifier.box.ProgramId
import co.topl.nodeView.NodeViewComponent
import co.topl.nodeView.state.MinimalState.VersionTag

import scala.util.Try

/**
 * Abstract functional interface of state which is a result of a sequential blocks applying
 */

trait MinimalState[M <: PersistentNodeViewModifier, MS <: MinimalState[M, MS]]
    extends NodeViewComponent
    with StateReader[ProgramId, Address] {

  self: MS =>

  def version: VersionTag

  def applyModifier(mod: M): Try[MS]

  def rollbackTo(version: VersionTag): Try[MS]

  def getReader: StateReader[ProgramId, Address] = this
}

object MinimalState {
  type VersionTag = ModifierId
}
