package co.topl.utils.mongodb.models

import co.topl.modifier.box.{AssetValue, SimpleValue, TokenValueHolder}

import java.nio.charset.StandardCharsets

sealed trait TokenValueDataModel

case class SimpleValueDataModel(quantity: String) extends TokenValueDataModel

case class AssetValueDataModel(
  assetCode:    AssetCodeDataModel,
  quantity:     String,
  securityRoot: String,
  metadata:     Option[String]
) extends TokenValueDataModel

object TokenValueDataModel {

  def apply(value: TokenValueHolder): TokenValueDataModel =
    value match {
      case SimpleValue(quantity) => SimpleValueDataModel(quantity.toString)
      case AssetValue(quantity, assetCode, securityRoot, metadata) =>
        AssetValueDataModel(
          AssetCodeDataModel(assetCode),
          quantity.toString,
          securityRoot.toString,
          metadata.map(data => new String(data.value, StandardCharsets.ISO_8859_1))
        )
    }
}
