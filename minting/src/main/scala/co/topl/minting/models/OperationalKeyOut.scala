package co.topl.minting.models

import co.topl.consensus.models._
import co.topl.models.Slot

case class OperationalKeyOut(
  slot:            Slot,
  childVK:         VerificationKeyEd25519,
  childSK:         SecretKeyEd25519,
  parentSignature: SignatureKesProduct,
  parentVK:        VerificationKeyKesProduct
)
