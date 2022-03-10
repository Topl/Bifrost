package co.topl.modifier.ops

import cats.implicits._
import co.topl.attestation.Address
import co.topl.models.Box.Values.Asset
import co.topl.models.utility.{Lengths, Sized}
import co.topl.modifier.box.AssetCode
import co.topl.modifier.transaction.builder.BuildTransferFailures
import co.topl.utils.StringDataTypes.Latin1Data
import co.topl.models.utility.StringDataTypes.{Latin1Data => TetraLatin1Data}
import co.topl.attestation.ops.implicits._
import co.topl.models.utility.HasLength.instances._

import scala.language.implicitConversions

class AssetCodeOps(private val value: AssetCode) extends AnyVal {

  import AssetCodeOps._

  def toTetraAssetCode: Either[ToTetraAssetCodeFailure, Asset.Code] =
    (
      value.issuer.toDionAddress.leftMap(_ => ToTetraAssetCodeFailures.InvalidAddress(value.issuer)),
      Sized
        .max[TetraLatin1Data, Lengths.`8`.type](
          TetraLatin1Data.fromData(value.shortName.value)
        )
        .leftMap(_ => ToTetraAssetCodeFailures.InvalidShortName(value.shortName))
    )
      .mapN((issuer, shortName) =>
        Asset.Code(
          value.version,
          issuer,
          shortName
        )
      )
}

object AssetCodeOps {
  sealed trait ToTetraAssetCodeFailure

  object ToTetraAssetCodeFailures {
    case class InvalidAddress(address: Address) extends ToTetraAssetCodeFailure
    case class InvalidShortName(shortName: Latin1Data) extends ToTetraAssetCodeFailure
  }

  trait ToAssetCodeOps {
    implicit def assetCodeOpsFromValue(value: AssetCode): AssetCodeOps = new AssetCodeOps(value)
  }

  trait Implicits extends ToAssetCodeOps

  object implicits extends Implicits
}
