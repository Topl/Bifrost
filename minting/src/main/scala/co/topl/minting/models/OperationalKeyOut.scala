package co.topl.minting.models

import co.topl.models.{Proofs, SecretKeys, Slot, VerificationKeys}

case class OperationalKeyOut(
  slot:            Slot,
  childVK:         VerificationKeys.Ed25519,
  childSK:         SecretKeys.Ed25519,
  parentSignature: Proofs.Knowledge.KesProduct,
  parentVK:        VerificationKeys.KesProduct
)
