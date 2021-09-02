package co.topl.consensus.vrf

import co.topl.crypto.signatures.Signature
import co.topl.crypto.signatures.Ed25519VRF

object ProofToHash {
  val vrf = new Ed25519VRF
  vrf.precompute()

  def digest(signature: Signature): Array[Byte] =
    vrf.vrfProofToHash(signature.value)
}
