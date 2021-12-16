package co.topl.models

import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, Sized}

case class Box[V <: Box.Value](evidence: Evidence, nonce: BoxNonce, value: V)

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
      case class Code(version: Byte, issuer: DionAddress, shortName: Sized.Max[Latin1Data, Lengths.`8`.type])
    }

    /**
     * @param commitment message: Hash(vrfVK | poolVK), SK: 0th timestep of the KES
     * @param activationSlot Slot in which this staker can start staking
     */
    case class TaktikosRegistration(
      commitment:     Proofs.Knowledge.KesProduct,
      activationSlot: Slot
    ) extends Value
  }
}
