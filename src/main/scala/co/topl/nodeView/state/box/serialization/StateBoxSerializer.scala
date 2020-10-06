package co.topl.nodeView.state.box.serialization

import co.topl.nodeView.state.ProgramId
import co.topl.nodeView.state.box.proposition.PublicKey25519PropositionSerializer
import co.topl.nodeView.state.box.{ ProgramBox, StateBox }
import co.topl.utils.serialization.{ BifrostSerializer, Reader, Writer }
import io.circe.{ Json, parser }

object StateBoxSerializer extends BifrostSerializer[StateBox] {

  override def serialize(obj: StateBox, w: Writer): Unit = {
    ProgramBoxSerializer.serialize(obj, w)

    /* state: Json, JSON representation of JS Variable Declarations */
    w.putIntString(obj.state.noSpaces)
  }

  override def parse(r: Reader): StateBox = {
    val (proposition, nonce, programId) = ProgramBoxSerializer.parse(r)

    val state: Json = parser.parse(r.getIntString()) match {
      case Left(f) => throw f
      case Right(j: Json) => j
    }

    StateBox(proposition, nonce, programId, state)
  }
}
