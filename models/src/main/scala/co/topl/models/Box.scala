package co.topl.models

import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, Sized}

import scala.util.Random

case class Box(evidence: TypedEvidence, nonce: BoxNonce, value: Box.Value)

object Box {
  sealed abstract class Value

  object Values {
    case object Empty extends Value
    case class Poly(value: Int128) extends Value
    case class Arbit(value: Int128) extends Value

    case class Asset(
      quantity:     Int128,
      assetCode:    Asset.Code,
      securityRoot: Bytes,
      metadata:     Option[Sized.Max[Latin1Data, Lengths.`127`.type]]
    ) extends Value

    object Asset {
      case class Code(version: Byte, issuer: DionAddress, shortName: Sized.Max[Latin1Data, Lengths.`8`.type])
    }

    /**
     * @param commitment message: Hash(vrfVK | poolVK), SK: 0th timestep of the KES
     */
    case class TaktikosRegistration(commitment: Proofs.Knowledge.KesProduct) extends Value
  }

  def apply(coinOutput: Transaction.CoinOutput): Box = coinOutput match {
    case Transaction.PolyOutput(dionAddress, value) =>
      Box(dionAddress.typedEvidence, Random.nextLong(), Box.Values.Poly(value))
    case Transaction.ArbitOutput(dionAddress, value) =>
      Box(dionAddress.typedEvidence, Random.nextLong(), Box.Values.Arbit(value))
    case Transaction.AssetOutput(dionAddress, value) => Box(dionAddress.typedEvidence, Random.nextLong(), value)
  }

  val empty: Box = Box(TypedEvidence.empty, 0, Box.Values.Empty)
}
