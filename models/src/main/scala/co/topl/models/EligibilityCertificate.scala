package co.topl.models

/**
 * @param vkVRF (Verification key VRF)
 * @param vrfNonceSig SIGMA "nonce": sign("nonce" + epochNonce, skVRF)
 * @param vrfTestSig SIGMA "test":  sign("test" + epochNonce, skVRF)
 */
case class EligibilityCertificate(
  vrfNonceSig:       Proofs.Signature.VrfEd25519,
  vrfTestSig:        Proofs.Signature.VrfEd25519,
  vkVRF:             VerificationKeys.Vrf,
  thresholdEvidence: Digest32
)
