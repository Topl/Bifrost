package co.topl.models

/**
 * @param vkVRF (Verification key VRF)
 * @param nonceProof SIGMA "nonce": sign("nonce" + epochNonce, skVRF)
 * @param testProof SIGMA "test":  sign("test" + epochNonce, skVRF)
 */
case class VrfCertificate(
  vkVRF:      PublicKeys.Vrf,
  nonceProof: Proofs.Consensus.Nonce,
  testProof:  Proofs.Consensus.VrfTest
)
