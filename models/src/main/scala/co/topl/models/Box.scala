package co.topl.models

import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, Sized}

case class Box[V <: Box.Value](evidence: Evidence, nonce: Nonce, value: V)

object Box {
  sealed abstract class Value

  object Values {
    case class Poly(value: Int128) extends Value
    case class Arbit(value: Int128) extends Value

    case class Asset(
      quantity:     Int128,
      assetCode:    Asset.Code,
      securityRoot: Bytes,
      metadata:     Option[Sized.Max[Latin1Data, Lengths.`127`.type]]
    ) extends Value

    object Asset {
      case class Code(version: Byte, issuer: Address, shortName: String)
    }

    // Note: We don't need to worry about these for phase 0
    case class TaktikosRegistration(registration: Registration, signature: Signature) extends Value
    case class TaktikosDelegation(address: TaktikosAddress) extends Value
  }
}
