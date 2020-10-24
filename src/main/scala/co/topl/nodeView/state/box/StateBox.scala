package co.topl.nodeView.state.box

import co.topl.crypto.proposition.PublicKey25519Proposition
import co.topl.nodeView.state.ProgramId
import io.circe.syntax._
import io.circe.{ Decoder, Encoder, HCursor, Json }

case class StateBox(override val proposition: PublicKey25519Proposition,
                    override val nonce: Long,
                    override val value: ProgramId,
                    state: Json //  JSON representation of JS Variable Declarations
                    ) extends ProgramBox(proposition, nonce, value) {

  override lazy val typeOfBox: String = "StateBox"
}

object StateBox {

  implicit val jsonEncoder: Encoder[StateBox] = { box: StateBox =>
    (ProgramBox.jsonEncode(box) ++ Map(
      "state" -> box.state.asJson,
    )).asJson
  }

  implicit val jsonDecoder: Decoder[StateBox] = ( c: HCursor ) =>
    for {
      b <- ProgramBox.jsonDecode(c)
      state <- c.downField("state").as[Json]
    } yield {
      val (proposition, nonce, programId) = b
      StateBox(proposition, nonce, programId, state)
    }
}
