package co.topl.models

import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, Sized}

import scala.util.Random

case class Box[V <: Box.Value](evidence: TypedEvidence, nonce: BoxNonce, value: V, data: Int)

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

    // Note: We don't need to worry about these for phase 0
    case class TaktikosRegistration(
      vrfCommitment:    Sized.Strict[Bytes, Lengths.`32`.type],
      extendedVk:       VerificationKeys.ExtendedEd25519,
      registrationSlot: Slot
    ) extends Value
    case class TaktikosDelegation(address: TaktikosAddress) extends Value
  }

  def apply(coinOutput: Transaction.CoinOutput): Box[_] = coinOutput match {
    case Transaction.PolyOutput(dionAddress, value) =>
      Box(dionAddress.typedEvidence, Random.nextLong(), Box.Values.Poly(value), 0)
    case Transaction.ArbitOutput(dionAddress, value) =>
      Box(dionAddress.typedEvidence, Random.nextLong(), Box.Values.Arbit(value), 0)
    case Transaction.AssetOutput(dionAddress, value) => Box(dionAddress.typedEvidence, Random.nextLong(), value, 0)
  }

  val empty: Box[Box.Values.Empty.type] = Box(TypedEvidence.empty, 0, Box.Values.Empty, 0)
}
