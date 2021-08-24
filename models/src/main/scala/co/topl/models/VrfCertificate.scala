package co.topl.models

import co.topl.models.utility.{Lengths, Sized}

/**
 * @param vkVRF (Verification key VRF)
 * @param nonceProof SIGMA "nonce": sign("nonce" + epochNonce, skVRF)
 * @param testProof SIGMA "test":  sign("test" + epochNonce, skVRF)
 */
case class VrfCertificate(
  vkVRF:      PublicKeys.Vrf,
  nonceProof: Sized.Strict[Bytes, Lengths.`64`.type],
  // TODO: Verify the length
  testProof: Sized.Strict[Bytes, Lengths.`80`.type]
)
