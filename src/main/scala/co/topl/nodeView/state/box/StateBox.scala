package co.topl.nodeView.state.box

import co.topl.nodeView.state.ProgramId
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
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

  implicit val jsonEncoder: Encoder[StateBox] = (box: StateBox) =>
    Map(
      "id" -> box.id.asJson,
      "type" -> box.typeOfBox.asJson,
      "proposition" -> box.proposition.asJson,
      "programId" -> box.value.asJson,
      "state" -> box.state.asJson,
      "nonce" -> box.nonce.asJson,
      ).asJson

  implicit val jsonDecoder: Decoder[StateBox] = (c: HCursor) =>
    for {
      proposition <- c.downField("proposition").as[PublicKey25519Proposition]
      value <- c.downField("programId").as[ProgramId]
      state <- c.downField("state").as[Json]
      nonce <- c.downField("nonce").as[Long]
    } yield {
      StateBox(proposition, nonce, value, state)
    }
}
