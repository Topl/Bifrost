package co.topl.models

case class OperationalCertificate(
  opSig: Proofs.Signature.HdKesSymProd,
  xvkM:  VerificationKeys.ExtendedEd25519,
  slotR: Slot
)
