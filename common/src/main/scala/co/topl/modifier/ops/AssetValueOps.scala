package co.topl.modifier.ops

import cats.implicits._
import co.topl.attestation.Address
import co.topl.attestation.ops.AddressOps.ToDionAddressFailure
import co.topl.models.{Box, Bytes, DionAddress, Transaction}
import co.topl.models.utility.HasLength.instances.{bigIntLength, latin1DataLength}
import co.topl.models.utility.{Lengths, Sized}
import co.topl.modifier.box.AssetValue
import co.topl.utils.{Int128 => DionInt128}
import co.topl.attestation.ops.AddressOps.implicits._
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.utils.StringDataTypes.{Latin1Data => DionLatin1Data}

import scala.language.implicitConversions

class AssetValueOps(private val assetValue: AssetValue) extends AnyVal {
  import AssetValueOps._

  /**
   * Attempts to convert a Dion [[AssetValue]] into a Tetra [[Transaction.Output]] with a given [[DionAddress]].
   *
   * @param address the address to use in the asset output value
   * @return a [[Transaction.Output]] if successful, otherwise a [[ToAssetOutputFailure]]
   */
  def toAssetOutput(address: DionAddress, minting: Boolean): ToAssetOutputResult[Transaction.Output] =
    for {
      quantity <-
        Sized
          .max[BigInt, Lengths.`128`.type](assetValue.quantity.toLong)
          .leftMap(error => ToAssetOutputFailures.InvalidQuantity(assetValue.quantity, error))
      issuer <-
        assetValue.assetCode.issuer.toDionAddress
          .leftMap[ToAssetOutputFailure](error =>
            ToAssetOutputFailures.InvalidIssuerAddress(assetValue.assetCode.issuer, error)
          )
      shortName <-
        Sized
          .max[Latin1Data, Lengths.`8`.type](Latin1Data.fromData(assetValue.assetCode.shortName.value))
          .leftMap[ToAssetOutputFailure](error =>
            ToAssetOutputFailures.InvalidShortName(assetValue.assetCode.shortName, error)
          )
      assetCode = Box.Values.Asset.Code(assetValue.assetCode.version, issuer, shortName)
      securityRoot = Bytes(assetValue.securityRoot.root)
      metadata <-
        assetValue.metadata.traverse[ToAssetOutputResult, Sized.Max[Latin1Data, Lengths.`127`.type]](data =>
          Sized
            .max[Latin1Data, Lengths.`127`.type](Latin1Data.fromData(data.value))
            .leftMap[ToAssetOutputFailure](error => ToAssetOutputFailures.InvalidMetadata(data, error))
        )
      asset = Box.Values.Asset(quantity, assetCode, securityRoot, metadata)
    } yield Transaction.Output(address, asset, minting)
}

object AssetValueOps {
  sealed trait ToAssetOutputFailure

  object ToAssetOutputFailures {
    case class InvalidQuantity(quantity: DionInt128, inner: Sized.InvalidLength) extends ToAssetOutputFailure
    case class InvalidIssuerAddress(isser: Address, inner: ToDionAddressFailure) extends ToAssetOutputFailure
    case class InvalidShortName(shortName: DionLatin1Data, inner: Sized.InvalidLength) extends ToAssetOutputFailure
    case class InvalidMetadata(metadata: DionLatin1Data, inner: Sized.InvalidLength) extends ToAssetOutputFailure
  }

  type ToAssetOutputResult[T] = Either[ToAssetOutputFailure, T]

  trait ToAssetValueOps {
    implicit def assetValueOpsFromAssetValue(assetValue: AssetValue): AssetValueOps = new AssetValueOps(assetValue)
  }

  trait Implicits extends ToAssetValueOps

  object implicits extends Implicits
}
