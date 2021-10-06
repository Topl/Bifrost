package co.topl.models

case class OperationalCertificate(
  opSig: Proofs.Signature.HdKes,
  xvkM:  VerificationKeys.ExtendedEd25519,
  slotR: Slot
)
