package co.topl.models

case class OperationalCertificate(
  opSig:             Proofs.Knowledge.Ed25519,
  linearVK:          VerificationKeys.Ed25519,
  linearVKSignature: Proofs.Knowledge.KesProduct
)
