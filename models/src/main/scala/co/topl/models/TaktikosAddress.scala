package co.topl.models

import co.topl.models.utility.{Lengths, Sized}

/**
 * @param paymentVKEvidence Evidence of the staker's payment key (payVK)
 * @param poolVK The VK of the pool in which this staker is participating
 * @param signature A commitment from the staker (paySK) to the poolVK
 */
case class TaktikosAddress(
  paymentVKEvidence: Evidence,
  poolVK:            VerificationKeys.Ed25519,
  signature:         Proofs.Knowledge.Ed25519
)
