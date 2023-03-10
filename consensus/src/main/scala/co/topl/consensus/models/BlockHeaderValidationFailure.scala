package co.topl.consensus.models

import co.topl.{models => legacyModels}
import legacyModels.Bytes
import legacyModels.Eta
import legacyModels.Slot
import legacyModels.Timestamp
import legacyModels.utility.Ratio

sealed abstract class BlockHeaderValidationFailure

object BlockHeaderValidationFailures {
  case class NonForwardSlot(slot: Slot, parentSlot: Slot) extends BlockHeaderValidationFailure

  case class NonForwardTimestamp(timestamp: Timestamp, parentTimestamp: Timestamp) extends BlockHeaderValidationFailure

  case class NonForwardHeight(height: Long, parentHeight: Long) extends BlockHeaderValidationFailure

  case class TimestampSlotMismatch(blockSlot: Slot, timestamp: Timestamp) extends BlockHeaderValidationFailure

  case class SlotBeyondForwardBiasedSlotWindow(globalSlot: Slot, blockSlot: Slot) extends BlockHeaderValidationFailure

  case class ParentMismatch(expectedParentId: BlockId, parentId: BlockId) extends BlockHeaderValidationFailure

  case class InvalidVrfThreshold(threshold: Ratio) extends BlockHeaderValidationFailure

  case class IneligibleCertificate(
    threshold:              Ratio,
    eligibilityCertificate: EligibilityCertificate
  ) extends BlockHeaderValidationFailure

  case class InvalidEligibilityCertificateEta(claimedEta: Eta, actualEta: Eta) extends BlockHeaderValidationFailure

  case class InvalidEligibilityCertificateProof(proof: Bytes) extends BlockHeaderValidationFailure

  case class InvalidEligibilityCertificateNonceProof(proof: Bytes) extends BlockHeaderValidationFailure

  case class InvalidOperationalParentSignature(operationalCertificate: OperationalCertificate)
      extends BlockHeaderValidationFailure

  case class InvalidBlockProof(operationalCertificate: OperationalCertificate) extends BlockHeaderValidationFailure

  case class Unregistered(address: StakingAddress) extends BlockHeaderValidationFailure

  case class RegistrationCommitmentMismatch(
    vrfCommitment: SignatureKesProduct,
    vrfVK:         Bytes,
    poolVK:        StakingAddress
  ) extends BlockHeaderValidationFailure

}
