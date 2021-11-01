package co.topl.consensus

import co.topl.models._
import co.topl.models.utility.{Lengths, Ratio, Sized}

sealed abstract class BlockHeaderValidationFailure

object BlockHeaderValidationFailures {
  case class NonForwardSlot(slot: Slot, parentSlot: Slot) extends BlockHeaderValidationFailure

  case class NonForwardTimestamp(timestamp: Timestamp, parentTimestamp: Timestamp) extends BlockHeaderValidationFailure

  case class NonForwardHeight(height: Long, parentHeight: Long) extends BlockHeaderValidationFailure

  case class ParentMismatch(expectedParentId: TypedIdentifier, parentId: TypedIdentifier)
      extends BlockHeaderValidationFailure

  case class InvalidVrfThreshold(threshold: Ratio) extends BlockHeaderValidationFailure

  case class IneligibleCertificate(threshold: Ratio, eligibilityCertificate: EligibilityCertificate)
      extends BlockHeaderValidationFailure

  case class InvalidEligibilityCertificateEta(claimedEta: Eta, actualEta: Eta) extends BlockHeaderValidationFailure

  case class InvalidEligibilityCertificateTestProof(proof: Proofs.Signature.VrfEd25519)
      extends BlockHeaderValidationFailure

  case class InvalidEligibilityCertificateNonceProof(proof: Proofs.Signature.VrfEd25519)
      extends BlockHeaderValidationFailure

  case class InvalidKesCertificateKESProof(kesCertificate: OperationalCertificate) extends BlockHeaderValidationFailure

  case class InvalidKesCertificateMMMProof(kesCertificate: OperationalCertificate) extends BlockHeaderValidationFailure

  case class Unregistered(address: TaktikosAddress) extends BlockHeaderValidationFailure

  case class RegistrationCommitmentMismatch(
    vrfCommitment: Sized.Strict[Bytes, Lengths.`32`.type],
    vkVrf:         VerificationKeys.VrfEd25519
  ) extends BlockHeaderValidationFailure

}
