package co.topl.models

import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, ReplaceModelUtil, Sized}
import co.topl.consensus.models.SignatureKesProduct

case class Box(evidence: TypedEvidence, value: Box.Value)

object Box {

  case class Id(transactionId: TypedIdentifier, transactionOutputIndex: Short)

  sealed abstract class Value

  object Values {
    case object Empty extends Value
    case class Poly(quantity: Int128) extends Value
    case class Arbit(quantity: Int128) extends Value

    case class AssetV1(
      quantity:     Int128,
      assetCode:    AssetV1.Code,
      securityRoot: AssetV1.SecurityRoot,
      metadata:     Option[AssetV1.Metadata]
    ) extends Value

    object AssetV1 {
      case class Code(issuer: SpendingAddress, shortName: Code.ShortName)

      object Code {
        type ShortName = Sized.Max[Latin1Data, Lengths.`8`.type]
      }

      type SecurityRoot = Sized.Strict[Bytes, Lengths.`32`.type]
      type Metadata = Sized.Max[Latin1Data, Lengths.`127`.type]
    }

    sealed abstract class Registration extends Value

    object Registrations {

      /**
       * Represents the registration of a stake pool operator.  Stake pool operators mint the actual blocks.
       * @param vrfCommitment A commitment  to the VRF.
       *                      signer: the operational key (KES parentSK at timestep=0)
       *                      message: Hash(vrfVK | poolVK)
       */
      case class Operator(vrfCommitment: Proofs.Knowledge.KesProduct) extends Registration {

        /**
         * TODO remove this conversion, when the old model is raplaced
         * @return
         */
        def toConsensusModel: OperatorNewModel = OperatorNewModel(
          ReplaceModelUtil.signatureKesProduct(vrfCommitment)
        )

      }
      case class OperatorNewModel(vrfCommitment: SignatureKesProduct) extends Registration

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
