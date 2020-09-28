package bifrost

import bifrost.modifier.ModifierId
import bifrost.modifier.box.Box
import com.google.common.primitives.Ints

import scala.util.{Try, Success, Failure}

package object state {
  def updateStorage(version: Int, boxes: Set[Box], state: State): Unit = {
    // Manually manipulate state
    val boxSC = StateChanges(Set(), boxes)
    val versionId = ModifierId(Ints.toByteArray(version))

    // this works by updating the underlying storage object directly and ignoring the updated state instance
    state.applyChanges(versionId, boxSC) match {
      case Success(_) => Unit
      case Failure(ex)    => throw ex
    }
  }
}
