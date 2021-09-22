package co.topl.models

/**
 * blockHeader.messageToSign
 */
case class KesCertificate(
  vkKES:    VerificationKeys.Kes, // vkL
  vkHD:     VerificationKeys.Ed25519, // vkI
  kesProof: Proofs.Signature.Ed25519,
  mmmProof: Proofs.Consensus.MMM
)
