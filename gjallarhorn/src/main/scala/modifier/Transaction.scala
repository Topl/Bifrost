package modifier

/**
 * This class is used to encode and decode transactions received from Bifrost
 * @param newBoxes the set of new boxes found in the received transaction
 * @param boxesToRemove the set of boxes to remove found in the received transaction
 */
case class Transaction(newBoxes: Seq[Box], boxesToRemove: Option[Seq[BoxId]])

object Transaction {

  implicit val txDecoder: Decoder[Transaction] = (hCursor: HCursor) => {
    for {
      newBoxes      <- hCursor.downField("newBoxes").as[Seq[Box]]
      boxesToRemove <- hCursor.downField("boxesToRemove").as[Option[Seq[BoxId]]]
    } yield Transaction(newBoxes, boxesToRemove)
  }

  implicit val txEncoder: Encoder[Transaction] = (tx: Transaction) =>
    Map(
      "newBoxes"      -> tx.newBoxes.asJson,
      "boxesToRemove" -> tx.boxesToRemove.asJson
    ).asJson

}

/**
 * A box for a given asset used to fund transactions or newly created to send assets
 * @param evidence the evidence is used to lock a box [[Evidence]]
 * @param nonce random number to ensure uniqueness
 * @param typeOfBox this defines the type of asset held in the box. Either "ArbitBox, "PolyBox", or "AssetBox"
 * @param value the value of the box
 */
case class Box(evidence: Evidence, nonce: Long, typeOfBox: String, value: TokenValueHolder) extends BytesSerializable {

  lazy val id: BoxId = BoxId(this)
  override type M = Box

  override def serializer: GjalSerializer[Box] = BoxSerializer
}

object Box {

  def typePrefix(box: Box): Byte = box.typeOfBox match {
    case "ArbitBox" => 1: Byte
    case "PolyBox"  => 2: Byte
    case "AssetBox" => 3: Byte
  }

  implicit val newBoxEncoder: Encoder[Box] = (box: Box) =>
    Map(
      "id"       -> box.id.asJson,
      "type"     -> box.typeOfBox.asJson,
      "evidence" -> box.evidence.toString.asJson,
      "value"    -> box.value.asJson,
      "nonce"    -> box.nonce.toString.asJson
    ).asJson

  implicit val newBoxDecoder: Decoder[Box] = (hCursor: HCursor) => {
    for {
      nonce     <- hCursor.downField("nonce").as[Long]
      typeOfBox <- hCursor.downField("type").as[String]
      evidence  <- hCursor.downField("evidence").as[Evidence]
      value     <- hCursor.downField("value").as[TokenValueHolder]
    } yield Box(evidence, nonce, typeOfBox, value)
  }
}
