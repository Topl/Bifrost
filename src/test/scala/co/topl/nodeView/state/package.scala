package co.topl.nodeView

import co.topl.modifier.ModifierId
import co.topl.nodeView.state.box.{ Box, ProgramBox, TokenBox }
import com.google.common.primitives.Ints
import scorex.crypto.hash.Blake2b256

import scala.util.{ Failure, Success }

package object state {
  /** This function will modify the state storage directly without returning a new instance of state
   * USE WITH EXTREME CAUTION!! */
  def directlyAddStateStorage( version: Int, boxes: Seq[Box], state: State): Unit = {
    // Manually manipulate state
    val boxSC = StateChanges(Set(), boxes.toSet)
    val versionId = ModifierId(Blake2b256(Ints.toByteArray(version)))

    // this works by updating the underlying storage object directly and ignoring the updated state instance
    state.applyChanges(versionId, boxSC) match {
      case Success(_) => Unit
      case Failure(ex)    => throw ex
    }
  }

  /** This function will modify the PBR storage directly without returning a new instance of the registry
   * USE WITH EXTREME CAUTION!! */
  def directlyAddPBRStorage ( version: Int, boxes: Seq[ProgramBox], state: State): Unit = {
    // Manually manipulate state
    val versionId = ModifierId(Blake2b256(Ints.toByteArray(version)))
    val updates = boxes.map(bx => bx.value -> Seq(bx.id)).toMap
    val pbrSC = ProgramRegistryChanges(Map(), updates)

    // this works by updating the underlying storage object directly and ignoring the updated state instance
    directlyAddStateStorage(version, boxes, state)
    state.pbrOpt.get.update(versionId, pbrSC.toRemove, pbrSC.toUpdate) match {
      case Success(_) => Unit
      case Failure(ex)  => throw ex
    }
  }

  /** This function will modify the PBR storage directly without returning a new instance of the registry
   * USE WITH EXTREME CAUTION!! */
  def directlyAddTBRStorage ( version: Int, boxes: Seq[TokenBox], state: State): Unit = {
    // Manually manipulate state
    val versionId = ModifierId(Blake2b256(Ints.toByteArray(version)))
    val updates = boxes.map(bx => bx.proposition -> Seq(bx.nonce)).toMap
    val tbrSC = TokenRegistryChanges(Map(), updates)

    // this works by updating the underlying storage object directly and ignoring the updated state instance
    directlyAddStateStorage(version, boxes, state)
    state.tbrOpt.get.update(versionId, tbrSC.toRemove, tbrSC.toAppend) match {
      case Success(_) => Unit
      case Failure(ex)  => throw ex
    }
  }
}
