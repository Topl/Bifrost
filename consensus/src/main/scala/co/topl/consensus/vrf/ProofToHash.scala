package co.topl.consensus.vrf

import co.topl.crypto.signing.Ed25519VRF
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized

object ProofToHash {
  val vrf = new Ed25519VRF
  vrf.precompute()

  def digest(signature: Proofs.Signature.VrfEd25519): Rho =
    Sized.strictUnsafe(Bytes(vrf.vrfProofToHash(signature.bytes.data.toArray)))
}
