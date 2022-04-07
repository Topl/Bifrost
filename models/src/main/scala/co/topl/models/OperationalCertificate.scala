package co.topl.models

/**
 * @param parentVK  The VK of the parent (hour/minute hands)
 * @param parentSignature Sign(childVK, parentSK)
 * @param childVK The linear VK
 * @param childSignature The signature of the block
 */
case class OperationalCertificate(
  parentVK:        VerificationKeys.KesProduct,
  parentSignature: Proofs.Knowledge.KesProduct,
  childVK:         VerificationKeys.Ed25519,
  childSignature:  Proofs.Knowledge.Ed25519
)
