package co.topl.codecs.binary.legacy.modifier.box

import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.modifier.box.StateBox
import io.circe.{parser, Json}

object StateBoxSerializer extends BifrostSerializer[StateBox] {

  override def serialize(obj: StateBox, w: Writer): Unit = {
    ProgramBoxSerializer.serialize(obj, w)

    /* state: Json, JSON representation of JS Variable Declarations */
    w.putIntString(obj.state.noSpaces)
  }

  override def parse(r: Reader): StateBox = {
    val (evidence, nonce, programId) = ProgramBoxSerializer.parse(r)

    val state: Json = parser.parse(r.getIntString()) match {
      case Left(f)        => throw f
      case Right(j: Json) => j
    }

    StateBox(evidence, nonce, programId, state)
  }
}
