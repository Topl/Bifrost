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
      case class Code(version: Byte, issuer: SpendingAddress, shortName: Sized.Max[Latin1Data, Lengths.`8`.type])
    }

    sealed abstract class Registration extends Value

    object Registrations {

      /**
       * Represents the registration of a stake pool operator.  Stake pool operators mint the actual blocks.
       * @param vrfCommitment A commitment  to the VRF.
       *                      signer: the operational key (KES parentSK at timestep=0)
       *                      message: Hash(vrfVK | poolVK)
       */
      case class Pool(vrfCommitment: Proofs.Knowledge.KesProduct) extends Registration

      /**
       * Represents the registration of someone intending to delegate their stake to a stake pool operator.  Owners
       * of these boxes do not produce blocks directly; they instead allow a stake pool operator to use their stake
       * to increase the likelihood of eligibility.  Delegaters receive rewards for delegation.
       *
       * NOTE: Delegation is currently not enabled
       *
       * @param poolAddress The address of the pool to which the user is delegating
       * @param rewardsAddress The address of rewards
       */
//      case class Delegating(poolAddress: StakingAddress, rewardsAddress: StakingAddress) extends Registration
    }
  }

  val empty: Box = Box(TypedEvidence.empty, Box.Values.Empty)
}
