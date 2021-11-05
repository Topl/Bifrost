package co.topl.consensus.vrf

import co.topl.crypto.signing.Ed25519VRF
import co.topl.models._

object ProofToHash {
  val vrf = new Ed25519VRF

  def digest(signature: Proofs.Signature.VrfEd25519): Rho = vrf.proofToHash(signature)
}
