package co.topl.models

import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, Sized}

case class Box(evidence: TypedEvidence, value: Box.Value)

object Box {
  sealed abstract class Value

  object Values {
    case object Empty extends Value
    case class Poly(value: Int128) extends Value
    case class Arbit(value: Int128) extends Value

    // TODO: AssetV1
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

  val empty: Box = Box(TypedEvidence.empty, Box.Values.Empty)
}
