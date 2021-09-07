package co.topl.consensus.vrf

import co.topl.crypto.signatures.Signature
import co.topl.crypto.signatures.Ed25519VRF
import co.topl.models.Proofs
object ProofToHash {
  val vrf = new Ed25519VRF
  vrf.precompute()

  def digest(signature: Proofs.Consensus.VrfTest): Array[Byte] =
    vrf.vrfProofToHash(signature.bytes.data.toArray)
  def digest(signature: Proofs.Consensus.Nonce): Array[Byte] =
    vrf.vrfProofToHash(signature.bytes.data.toArray)
}
