package crypto

import io.circe.{Decoder, Encoder, HCursor}
import io.circe.syntax.EncoderOps

case class Transaction(newBoxes: Seq[NewBox], boxesToRemove: Option[Seq[String]])

object Transaction {
  implicit val txDecoder: Decoder[Transaction] = (hCursor: HCursor) => {
    for {
      newBoxes <- hCursor.downField("newBoxes").as[Seq[NewBox]]
      boxesToRemove <- hCursor.downField("boxesToRemove").as[Option[Seq[String]]]
    } yield Transaction(newBoxes, boxesToRemove)
  }
}


case class NewBox(nonce: String,
                    id: String,
                    typeOfBox: String,
                    proposition: PublicKey25519Proposition,
                    value: Long)

object NewBox {
  implicit val newBoxEncoder: Encoder[NewBox] = (box: NewBox) =>
  Map(
      "id" -> box.id.toString.asJson,
      "type" -> box.typeOfBox.asJson,
      "proposition" -> box.proposition.toString.asJson,
      "value" -> box.value.toString.asJson,
      "nonce" -> box.nonce.toString.asJson
    ).asJson

  implicit val newBoxDecoder: Decoder[NewBox] = (hCursor: HCursor) => {
    for {
      nonce <- hCursor.downField("nonce").as[String]
      id <- hCursor.downField("id").as[String]
      typeOfBox <- hCursor.downField("type").as[String]
      proposition <- hCursor.downField("proposition").as[PublicKey25519Proposition]
      value <- hCursor.downField("value").as[Long]
    } yield NewBox(nonce, id, typeOfBox, proposition, value)
  }
}
