package co.topl.models

import co.topl.models.utility.{Lengths, Sized}

/**
 * @param paymentVKEvidence Evidence of the staker's payment key (payVK)
 * @param poolVK The VK of the pool in which this staker is participating
 * @param signature A commitment from the staker (paySK) to the poolVK
 */
case class TaktikosAddress(
  paymentVKEvidence: Evidence, // todo: should be typed evidence, rename to spendingEvidence
  poolVK:            VerificationKeys.Ed25519, // todo: rename to stakingEvidence
  signature: Proofs.Knowledge.Ed25519 // todo: can this be a generic Proof or we can just put a fixed sized array
)
