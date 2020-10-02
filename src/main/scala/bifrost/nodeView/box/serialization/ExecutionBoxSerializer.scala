package bifrost.nodeView.box.serialization

import bifrost.nodeView.box.{ ExecutionBox, ProgramBox }
import bifrost.nodeView.state.ProgramId
import bifrost.utils.Extensions._
import bifrost.utils.serialization.{ BifrostSerializer, Reader, Writer }

object ExecutionBoxSerializer extends BifrostSerializer[ExecutionBox] {

  override def serialize(obj: ExecutionBox, w: Writer): Unit = {
    ProgramBoxSerializer.serialize(obj, w)

    /* stateBoxIds: Seq[ProgramId], List of program ids of state boxes in ProgramBoxRegistry */
    w.putUInt(obj.stateBoxIds.length)
    obj.stateBoxIds.foreach { id =>
      ProgramId.serialize(id, w)
    }

    /* codeBoxIds: Seq[ProgramId] */
    w.putUInt(obj.codeBoxIds.length)
    obj.codeBoxIds.foreach{id =>
      ProgramId.serialize(id, w)
    }
  }

  override def parse(r: Reader): ExecutionBox = {
    val programBox: ProgramBox = ProgramBoxSerializer.parse(r)

    /* stateBoxIds: Seq[ProgramId], List of program ids of state boxes from ProgramBoxRegistry */
    val stateBoxIdsLength: Int = r.getUInt().toIntExact
    val stateBoxIds: Seq[ProgramId] = (0 until stateBoxIdsLength).map(_ => ProgramId.parse(r))

    /* codeBoxIds: Seq[ProgramId] */
    val codeBoxIdsLength: Int = r.getUInt().toIntExact
    val codeBoxIds: Seq[ProgramId] = (0 until codeBoxIdsLength).map(_ => ProgramId.parse(r))

    ExecutionBox(programBox.proposition, programBox.nonce, programBox.value, stateBoxIds, codeBoxIds)
  }
}
