package co.topl.models

import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, Sized}

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
         * TODO remove this conversion, when the old model is raplaces
         * @return
         */
        def toConsensusModel: OperatorNewModel =
          OperatorNewModel(
            co.topl.consensus.models.SignatureKesProduct(
              superSignature = Option(
                co.topl.consensus.models.SignatureKesSum(
                  verificationKey = Some(
                    co.topl.crypto.models.VerificationKeyEd25519(
                      com.google.protobuf.ByteString
                        .copyFrom(this.vrfCommitment.superSignature.verificationKey.bytes.data.toArray)
                    )
                  ),
                  signature = Some(
                    co.topl.crypto.models.SignatureEd25519(
                      com.google.protobuf.ByteString
                        .copyFrom(this.vrfCommitment.superSignature.signature.bytes.data.toArray)
                    )
                  ),
                  witness = this.vrfCommitment.superSignature.witness.map(w =>
                    com.google.protobuf.ByteString.copyFrom(w.data.toArray)
                  ),
                  unknownFields = scalapb.UnknownFieldSet.empty
                )
              ),
              subSignature = Option(
                co.topl.consensus.models.SignatureKesSum(
                  verificationKey = Some(
                    co.topl.crypto.models.VerificationKeyEd25519(
                      com.google.protobuf.ByteString
                        .copyFrom(this.vrfCommitment.subSignature.verificationKey.bytes.data.toArray)
                    )
                  ),
                  signature = Some(
                    co.topl.crypto.models.SignatureEd25519(
                      com.google.protobuf.ByteString
                        .copyFrom(this.vrfCommitment.subSignature.signature.bytes.data.toArray)
                    )
                  ),
                  witness = this.vrfCommitment.subSignature.witness.map(w =>
                    com.google.protobuf.ByteString.copyFrom(w.data.toArray)
                  ),
                  unknownFields = scalapb.UnknownFieldSet.empty
                )
              ),
              subRoot = com.google.protobuf.ByteString.copyFrom(this.vrfCommitment.subRoot.data.toArray),
              unknownFields = scalapb.UnknownFieldSet.empty
            )
          )
      }
      case class OperatorNewModel(vrfCommitment: co.topl.consensus.models.SignatureKesProduct) extends Registration

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
