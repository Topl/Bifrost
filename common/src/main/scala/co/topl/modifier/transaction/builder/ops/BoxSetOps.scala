package co.topl.modifier.transaction.builder.ops

import cats.implicits._
import co.topl.attestation.Address
import co.topl.attestation.implicits._
import co.topl.models.BoxReference
import co.topl.modifier.box.AssetCode
import co.topl.modifier.transaction.builder.BoxSet
import co.topl.utils.Int128

import scala.language.implicitConversions

class BoxSetOps(private val value: BoxSet) extends AnyVal {

  import BoxSetOps._

  def toBoxReferences: Either[BoxSetOps.ToBoxReferencesFailure, List[BoxReference]] =
    for {
      polyReferences <-
        value.polys.traverse(poly =>
          poly._1.toDionAddress
            .map(addr => addr -> poly._2.nonce)
            .leftMap(_ => ToBoxReferencesFailures.InvalidAddress(poly._1): ToBoxReferencesFailure)
        )
      arbitReferences <-
        value.arbits.traverse(arbit =>
          arbit._1.toDionAddress
            .map(addr => addr -> arbit._2.nonce)
            .leftMap(_ => ToBoxReferencesFailures.InvalidAddress(arbit._1): ToBoxReferencesFailure)
        )
      assetReferences <-
        value.assets.traverse(asset =>
          asset._1.toDionAddress
            .map(addr => addr -> asset._2.nonce)
            .leftMap(_ => ToBoxReferencesFailures.InvalidAddress(asset._1): ToBoxReferencesFailure)
        )
    } yield polyReferences ++ arbitReferences ++ assetReferences

  def polySum: Int128 = value.polys.map(_._2.value.quantity).sum

  def arbitSum: Int128 = value.arbits.map(_._2.value.quantity).sum

  def assetSum: Map[AssetCode, Int128] =
    value.assets.groupMapReduce(_._2.value.assetCode)(_._2.value.quantity)(_ + _)
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
