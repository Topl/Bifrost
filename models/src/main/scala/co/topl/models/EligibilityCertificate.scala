package co.topl.models

/**
 * @param vrfSig sign(eta ++ slot, skVRF)
 * @param vkVRF (Verification key VRF)
 * @param thresholdEvidence Hash of the threshold of the minter
 * @param eta The claimed eta by the minter
 */
case class EligibilityCertificate(
  vrfSig:            Proofs.Knowledge.VrfEd25519,
  vkVRF:             VerificationKeys.VrfEd25519,
  thresholdEvidence: Evidence,
  eta:               Eta
)
