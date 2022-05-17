package co.topl.modifier.transaction.builder.ops

import cats.implicits._
import co.topl.attestation.Address
import co.topl.attestation.implicits._
import co.topl.models.{BoxReference, DionAddress}
import co.topl.modifier.box.{AssetCode, Box}
import co.topl.modifier.transaction.builder.BoxSet
import co.topl.utils.Int128

import scala.language.implicitConversions

class BoxSetOps(private val value: BoxSet) extends AnyVal {

  import BoxSetOps._

  def toBoxReferences: Either[BoxSetOps.ToBoxReferencesFailure, Set[BoxReference]] =
    for {
      polyReferences <-
        value.polys.toList.traverse[Either[BoxSetOps.ToBoxReferencesFailure, *], (DionAddress, Box.Nonce)](poly =>
          poly._1.toDionAddress
            .map(addr => addr -> poly._2.nonce)
            .leftMap(_ => ToBoxReferencesFailures.InvalidAddress(poly._1): ToBoxReferencesFailure)
        )
      arbitReferences <-
        value.arbits.toList.traverse[Either[BoxSetOps.ToBoxReferencesFailure, *], (DionAddress, Box.Nonce)](arbit =>
          arbit._1.toDionAddress
            .map(addr => addr -> arbit._2.nonce)
            .leftMap(_ => ToBoxReferencesFailures.InvalidAddress(arbit._1): ToBoxReferencesFailure)
        )
      assetReferences <-
        value.assets.toList.traverse[Either[BoxSetOps.ToBoxReferencesFailure, *], (DionAddress, Box.Nonce)](asset =>
          asset._1.toDionAddress
            .map(addr => addr -> asset._2.nonce)
            .leftMap(_ => ToBoxReferencesFailures.InvalidAddress(asset._1): ToBoxReferencesFailure)
        )
    } yield polyReferences.toSet ++ arbitReferences.toSet ++ assetReferences.toSet

  def polySum: Int128 = value.polys.map(_._2.value.quantity).sum

  def arbitSum: Int128 = value.arbits.map(_._2.value.quantity).sum

  def assetSums: Map[AssetCode, Int128] =
    value.assets
      .map(asset => asset._2.value)
      .map(value => value.assetCode -> value.quantity)
      .toMap

  def polyNonces: Set[Box.Nonce] = value.polys.map(_._2.nonce)

  def arbitNonces: Set[Box.Nonce] = value.arbits.map(_._2.nonce)

  def assetNonces: Set[Box.Nonce] = value.assets.map(_._2.nonce)
}

object BoxSetOps {
  sealed trait ToBoxReferencesFailure

  object ToBoxReferencesFailures {
    final case class InvalidAddress(address: Address) extends ToBoxReferencesFailure
  }

  trait ToBoxSetOps {
    implicit def boxSetOpsFromBoxSet(value: BoxSet): BoxSetOps = new BoxSetOps(value)
  }

  trait Implicits extends ToBoxSetOps

  object implicits extends Implicits
}
