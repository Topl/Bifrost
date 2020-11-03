package co.topl.nodeView.state.box

import co.topl.attestation.Evidence
import co.topl.nodeView.state.box.Box.BoxType
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}

case class StateBox(override val evidence   : Evidence,
                    override val nonce      : Box.Nonce,
                    override val value      : ProgramId,
                    state: Json //  JSON representation of JS Variable Declarations
                    ) extends ProgramBox(evidence, nonce, value, StateBox.boxTypePrefix)

object StateBox {
  val boxTypePrefix: BoxType = 12: Byte

  implicit val jsonEncoder: Encoder[StateBox] = { box: StateBox =>
    (Box.jsonEncode(box) ++ Map(
      "state" -> box.state.asJson,
    )).asJson
  }

  implicit val jsonDecoder: Decoder[StateBox] = ( c: HCursor ) =>
    for {
      b <- Box.jsonDecode(c)
      state <- c.downField("state").as[Json]
    } yield {
      val (proposition, nonce, programId) = b
      StateBox(proposition, nonce, programId, state)
    }
}
