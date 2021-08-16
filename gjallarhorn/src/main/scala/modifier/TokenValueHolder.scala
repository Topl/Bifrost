package modifier

/**
 * An abstract class for the type of values (or tokens) held in boxes
 * @param quantity the quantity of the value held in the box
 */
sealed abstract class TokenValueHolder(val quantity: Long) extends BytesSerializable {
  override type M = TokenValueHolder

  override def serializer: GjalSerializer[TokenValueHolder] = TokenValueHolder
}

object TokenValueHolder extends GjalSerializer[TokenValueHolder] {

  implicit val jsonEncoder: Encoder[TokenValueHolder] = {
    case v: SimpleValue => SimpleValue.jsonEncoder(v)
    case v: AssetValue  => AssetValue.jsonEncoder(v)
    case _              => throw new Error(s"No matching encoder found")
  }

  implicit val jsonDecoder: Decoder[TokenValueHolder] = { c: HCursor =>
    c.downField("type").as[String].map {
      case SimpleValue.valueTypeString => SimpleValue.jsonDecoder(c)
      case AssetValue.valueTypeString  => AssetValue.jsonDecoder(c)
    } match {
      case Right(v) => v
      case Left(ex) => throw ex
    }
  }

  override def serialize(obj: TokenValueHolder, w: Writer): Unit =
    obj match {
      case obj: SimpleValue =>
        w.put(SimpleValue.valueTypePrefix)
        SimpleValue.serialize(obj, w)

      case obj: AssetValue =>
        w.put(AssetValue.valueTypePrefix)
        AssetValue.serialize(obj, w)

      case _ => throw new Exception("Unanticipated TokenValueType type")
    }

  override def parse(r: Reader): TokenValueHolder =
    r.getByte() match {
      case SimpleValue.valueTypePrefix => SimpleValue.parse(r)
      case AssetValue.valueTypePrefix  => AssetValue.parse(r)
      case _                           => throw new Exception("Unanticipated Box Type")
    }
}

/**
 * The [[TokenValueHolder]] for an "ArbitBox" or "PolyBox".
 * This token only contains a type and quantity
 * @param quantity the quantity of arbits or polys held in the box
 */
case class SimpleValue(override val quantity: Long) extends TokenValueHolder(quantity)

object SimpleValue extends GjalSerializer[SimpleValue] {
  val valueTypeString: String = "Simple"
  val valueTypePrefix: Byte = 1: Byte

  implicit val jsonEncoder: Encoder[SimpleValue] = (value: SimpleValue) =>
    Map(
      "type"     -> valueTypeString.asJson,
      "quantity" -> value.quantity.asJson
    ).asJson

  implicit val jsonDecoder: Decoder[SimpleValue] = (hCursor: HCursor) =>
    for {
      quantity <- hCursor.downField("quantity").as[Long]
    } yield SimpleValue(quantity)

  override def serialize(obj: SimpleValue, w: Writer): Unit =
    w.putULong(obj.quantity)

  override def parse(r: Reader): SimpleValue =
    SimpleValue(r.getULong())

}

/**
 * The [[TokenValueHolder]] for an "AssetBox"
 * @param quantity the quantity of the asset held in the box
 * @param assetCode an id that identifies the asset
 * @param securityRoot used to prove membership
 * @param metadata additional data for the given asset
 */
case class AssetValue(
  override val quantity: Long,
  assetCode:             AssetCode,
  securityRoot:          SecurityRoot = SecurityRoot.empty,
  metadata:              Option[String] = None
) extends TokenValueHolder(quantity)

object AssetValue extends GjalSerializer[AssetValue] {
  val valueTypeString: String = "Asset"
  val valueTypePrefix: Byte = 2: Byte

  implicit val jsonEncoder: Encoder[AssetValue] = { (value: AssetValue) =>
    Map(
      "type"         -> valueTypeString.asJson,
      "quantity"     -> value.quantity.asJson,
      "assetCode"    -> value.assetCode.asJson,
      "securityRoot" -> value.securityRoot.asJson,
      "metadata"     -> value.metadata.asJson
    ).asJson
  }

  implicit val jsonDecoder: Decoder[AssetValue] = (c: HCursor) =>
    for {
      quantity     <- c.downField("quantity").as[Long]
      assetCode    <- c.downField("assetCode").as[AssetCode]
      securityRoot <- c.downField("securityRoot").as[Option[Base58Data]]
      metadata     <- c.downField("metadata").as[Option[String]]
    } yield {
      val sr = securityRoot match {
        case Some(data) => SecurityRoot.fromBase58(data)
        case None       => SecurityRoot.empty
      }

      modifier.AssetValue(quantity, assetCode, sr, metadata)
    }

  override def serialize(obj: AssetValue, w: Writer): Unit = {
    w.putULong(obj.quantity)
    AssetCode.serialize(obj.assetCode, w)
    SecurityRoot.serialize(obj.securityRoot, w)
    w.putOption(obj.metadata) { (writer, metadata) =>
      writer.putByteString(metadata)
    }
  }

  override def parse(r: Reader): AssetValue = {
    val quantity = r.getULong()
    val assetCode = AssetCode.parse(r)
    val securityRoot = SecurityRoot.parse(r)
    val metadata: Option[String] = r.getOption {
      r.getByteString()
    }

    AssetValue(quantity, assetCode, securityRoot, metadata)
  }
}
