package crypto

import attestation.Evidence
import io.circe.{Decoder, Encoder, HCursor}
import io.circe.syntax.EncoderOps
import modifier.{BoxId, BoxSerializer}
import utils.serialization.{BytesSerializable, GjalSerializer}

case class Transaction(newBoxes: Seq[Box], boxesToRemove: Option[Seq[BoxId]])

object Transaction {

  implicit val txDecoder: Decoder[Transaction] = (hCursor: HCursor) => {
    for {
      newBoxes <- hCursor.downField("newBoxes").as[Seq[Box]]
      boxesToRemove <- hCursor.downField("boxesToRemove").as[Option[Seq[BoxId]]]
    } yield Transaction(newBoxes, boxesToRemove)
  }

  implicit val txEncoder: Encoder[Transaction] = (tx: Transaction) =>
    Map(
      "newBoxes" -> tx.newBoxes.asJson,
      "boxesToRemove" -> tx.boxesToRemove.asJson
    ).asJson

}

case class Box(evidence: Evidence,
               nonce: Long,
               typeOfBox: String,
               value: TokenValueHolder) extends BytesSerializable {

  lazy val id: BoxId = BoxId(this)
  override type M = Box

  override def serializer: GjalSerializer[Box] = BoxSerializer
}

object Box {
  def typePrefix(box: Box): Byte = box.typeOfBox match {
    case "ArbitBox" => 1: Byte
    case "PolyBox" => 2: Byte
    case "AssetBox" => 3: Byte
  }

  implicit val newBoxEncoder: Encoder[Box] = (box: Box) =>
    Map(
      "id" -> box.id.asJson,
      "type" -> box.typeOfBox.asJson,
      "evidence" -> box.evidence.toString.asJson,
      "value" -> box.value.asJson,
      "nonce" -> box.nonce.toString.asJson
    ).asJson

  implicit val newBoxDecoder: Decoder[Box] = (hCursor: HCursor) => {
    for {
      nonce <- hCursor.downField("nonce").as[Long]
      typeOfBox <- hCursor.downField("type").as[String]
      evidence <- hCursor.downField("evidence").as[Evidence]
      value <- hCursor.downField("value").as[TokenValueHolder]
    } yield Box(evidence, nonce, typeOfBox, value)
  }
}
