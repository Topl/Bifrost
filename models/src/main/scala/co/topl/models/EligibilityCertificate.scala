package co.topl.models

/**
 * @param vkVRF (Verification key VRF)
 * @param vrfNonceSig SIGMA "nonce": sign("nonce" + epochNonce, skVRF)
 * @param vrfTestSig SIGMA "test":  sign("test" + epochNonce, skVRF)
 * @param thresholdEvidence Hash of the threshold of the minter
 * @param eta The claimed eta by the minter
 * TODO: Combine thresholdEvidence + eta?
 */
case class EligibilityCertificate(
  vrfNonceSig:       Proofs.Knowledge.VrfEd25519,
  vrfTestSig:        Proofs.Knowledge.VrfEd25519,
  vkVRF:             VerificationKeys.VrfEd25519,
  thresholdEvidence: Evidence,
  eta:               Eta
)
