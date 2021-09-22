package co.topl.models

import co.topl.models.utility.Ratio

object Vrf {

  /**
   * @param vkVRF (Verification key VRF)
   * @param nonceProof SIGMA "nonce": sign("nonce" + epochNonce, skVRF)
   * @param testProof SIGMA "test":  sign("test" + epochNonce, skVRF)
   */
  case class Certificate(
    vkVRF:      VerificationKeys.Vrf,
    nonceProof: Proofs.Vrf.Nonce,
    testProof:  Proofs.Vrf.Test
  )

  case class Hit(cert: Certificate, slot: Slot, threshold: Ratio)

  case class Config(lddCutoff: Int, precision: Int, baselineDifficulty: Ratio, amplitude: Ratio)
}
