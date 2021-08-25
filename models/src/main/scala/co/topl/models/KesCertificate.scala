package co.topl.models

/**
 * blockHeader.messageToSign
 */
case class KesCertificate(
  vkKES:      PublicKeys.Kes,
  kesProof:   Proofs.Kes,
  blockProof: Proofs.Block,
  slotOffset: Long
)
