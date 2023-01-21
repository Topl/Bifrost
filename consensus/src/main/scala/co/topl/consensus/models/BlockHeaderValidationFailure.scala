package co.topl.consensus.models

import co.topl.{models => legacyModels}
import legacyModels.{Eta, Slot, StakingAddress, Timestamp, TypedIdentifier, VerificationKeys}
import legacyModels.utility.Ratio
import co.topl.consensus.models._

sealed abstract class BlockHeaderValidationFailure

object BlockHeaderValidationFailures {
  case class NonForwardSlot(slot: Slot, parentSlot: Slot) extends BlockHeaderValidationFailure

  case class NonForwardTimestamp(timestamp: Timestamp, parentTimestamp: Timestamp) extends BlockHeaderValidationFailure

  case class NonForwardHeight(height: Long, parentHeight: Long) extends BlockHeaderValidationFailure

  case class TimestampSlotMismatch(blockSlot: Slot, timestamp: Timestamp) extends BlockHeaderValidationFailure

  case class SlotBeyondForwardBiasedSlotWindow(globalSlot: Slot, blockSlot: Slot) extends BlockHeaderValidationFailure

  case class ParentMismatch(expectedParentId: TypedIdentifier, parentId: TypedIdentifier)
      extends BlockHeaderValidationFailure

  case class InvalidVrfThreshold(threshold: Ratio) extends BlockHeaderValidationFailure

  case object EmptyEligibilityCertificate extends BlockHeaderValidationFailure

  case class IneligibleCertificate(
    threshold:              Ratio,
    eligibilityCertificate: EligibilityCertificate
  ) extends BlockHeaderValidationFailure

  case class InvalidEligibilityCertificateEta(claimedEta: Eta, actualEta: Eta) extends BlockHeaderValidationFailure

  case class InvalidEligibilityCertificateProof(proof: SignatureVrfEd25519) extends BlockHeaderValidationFailure

  case class InvalidEligibilityCertificateNonceProof(proof: SignatureVrfEd25519) extends BlockHeaderValidationFailure

  case class InvalidOperationalParentSignature(operationalCertificate: OperationalCertificate)
      extends BlockHeaderValidationFailure

  case object EmptyOperationalCertificate extends BlockHeaderValidationFailure

  case class InvalidBlockProof(operationalCertificate: OperationalCertificate) extends BlockHeaderValidationFailure

  case class Unregistered(address: StakingAddress) extends BlockHeaderValidationFailure

  case class RegistrationCommitmentMismatch(
    vrfCommitment: SignatureKesProduct,
    vrfVK:         VerificationKeyVrfEd25519,
    poolVK:        VerificationKeys.Ed25519
  ) extends BlockHeaderValidationFailure

}
