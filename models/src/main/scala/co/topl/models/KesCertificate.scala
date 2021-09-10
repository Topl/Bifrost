package co.topl.models

/**
 * blockHeader.messageToSign
 */
case class KesCertificate(
  vkKES:    PublicKeys.Kes,
  kesProof: Proofs.Consensus.KesCertificate,
  mmmProof: Proofs.Consensus.MMM
)
