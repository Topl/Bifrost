package co.topl.modifier.box

import co.topl.attestation.Evidence
import co.topl.modifier.box.Box.BoxType
import co.topl.utils.{Identifiable, Identifier}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}

case class StateBox(
  override val evidence: Evidence,
  override val nonce:    Box.Nonce,
  override val value:    ProgramId,
  state:                 Json //  JSON representation of JS Variable Declarations
) extends ProgramBox(evidence, nonce, value)

object StateBox {
  val typePrefix: BoxType = 12: Byte
  val typeString: String = "StateBox"

  implicit val identifier: Identifiable[StateBox] = Identifiable.instance { () =>
    Identifier(typeString, typePrefix)
  }

  implicit val jsonEncoder: Encoder[StateBox] = { box: StateBox =>
    (Box.jsonEncode[ProgramId, StateBox](box) ++ Map(
      "state" -> box.state.asJson
    )).asJson
  }

  implicit val jsonDecoder: Decoder[StateBox] = (c: HCursor) =>
    for {
      b     <- Box.jsonDecode[ProgramId](c)
      state <- c.downField("state").as[Json]
    } yield {
      val (evidence, nonce, programId) = b
      StateBox(evidence, nonce, programId, state)
    }
}
