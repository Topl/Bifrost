package co.topl.nodeView.state.box

import co.topl.attestation.Evidence
import co.topl.nodeView.state.box.Box.BoxType
import co.topl.utils.Identifiable
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}

case class StateBox(override val evidence   : Evidence,
                    override val nonce      : Box.Nonce,
                    override val value      : ProgramId,
                    state: Json //  JSON representation of JS Variable Declarations
                    ) extends ProgramBox(evidence, nonce, value)

object StateBox {
  val boxTypePrefix: BoxType = 12: Byte
  val boxTypeString: String = "StateBox"

  implicit val identifier: Identifiable[StateBox] = new Identifiable[StateBox] {
    override def typePrefix: Byte = boxTypePrefix
    override def typeString: String = boxTypeString
  }

  implicit val jsonEncoder: Encoder[StateBox] = { box: StateBox =>
    (Box.jsonEncode[ProgramId, StateBox](box) ++ Map(
      "state" -> box.state.asJson,
    )).asJson
  }

  implicit val jsonDecoder: Decoder[StateBox] = ( c: HCursor ) =>
    for {
      b <- Box.jsonDecode[ProgramId](c)
      state <- c.downField("state").as[Json]
    } yield {
      val (evidence, nonce, programId) = b
      StateBox(evidence, nonce, programId, state)
    }
}
