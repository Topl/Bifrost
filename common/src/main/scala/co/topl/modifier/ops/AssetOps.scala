package co.topl.modifier.ops

import co.topl.attestation.{Address, Evidence}
import co.topl.models.Box
import co.topl.modifier.box.{AssetCode, AssetValue, SecurityRoot}
import co.topl.utils.Int128
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Latin1Data

import scala.language.implicitConversions

class AssetOps(private val asset: Box.Values.AssetV1) extends AnyVal {

  def toAssetValue(implicit networkPrefix: NetworkPrefix): AssetValue =
    AssetValue(
      Int128(asset.quantity.data),
      AssetCode(
        asset.assetCode.version,
        Address(Evidence(asset.assetCode.issuer.typedEvidence.allBytes.toArray)),
        Latin1Data.fromData(asset.assetCode.shortName.data.bytes)
      ),
      SecurityRoot(asset.securityRoot.data.toArray),
      asset.metadata.map(data => Latin1Data.fromData(data.data.bytes))
    )
}

object AssetOps {

  trait ToAssetOps {
    implicit def assetOpsFromAsset(asset: Box.Values.AssetV1): AssetOps = new AssetOps(asset)
  }

  trait Implicits extends ToAssetOps

  object implicits extends Implicits
}
