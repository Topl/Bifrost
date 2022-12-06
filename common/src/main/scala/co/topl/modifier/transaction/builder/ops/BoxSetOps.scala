package co.topl.modifier.transaction.builder.ops

import co.topl.attestation.Address
import co.topl.modifier.box.{AssetCode, Box}
import co.topl.modifier.transaction.builder.BoxMap
import co.topl.utils.Int128

import scala.language.implicitConversions

class BoxSetOps(private val value: BoxMap) extends AnyVal {

  import BoxSetOps._

  def polySum: Int128 = value.polys.map(_._2.value.quantity).sum

  def arbitSum: Int128 = value.arbits.map(_._2.value.quantity).sum

  def assetSums: Map[AssetCode, Int128] =
    value.assets
      .map(asset => asset._2.value)
      .map(value => value.assetCode -> value.quantity)
      .toMap

  def polyNonces: List[Box.Nonce] = value.polys.map(_._2.nonce)

  def arbitNonces: List[Box.Nonce] = value.arbits.map(_._2.nonce)

  def assetNonces: List[Box.Nonce] = value.assets.map(_._2.nonce)
}

object BoxSetOps {
  sealed trait ToBoxReferencesFailure

  object ToBoxReferencesFailures {
    final case class InvalidAddress(address: Address) extends ToBoxReferencesFailure
  }

  trait ToBoxSetOps {
    implicit def boxSetOpsFromBoxSet(value: BoxMap): BoxSetOps = new BoxSetOps(value)
  }

  trait Implicits extends ToBoxSetOps

  object implicits extends Implicits
}
