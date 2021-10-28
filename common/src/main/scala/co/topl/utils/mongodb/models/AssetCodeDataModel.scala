package co.topl.utils.mongodb.models

import co.topl.modifier.box.AssetCode

import java.nio.charset.StandardCharsets

case class AssetCodeDataModel(assetCodeVersion: Int, issuer: String, shortName: String)

object AssetCodeDataModel {

  def apply(assetCode: AssetCode): AssetCodeDataModel =
    AssetCodeDataModel(
      assetCode.version,
      assetCode.issuer.toString,
      new String(assetCode.shortName.value, StandardCharsets.ISO_8859_1)
    )
}
