package co.topl.consensus.vrf

import co.topl.crypto.signatures.Ed25519VRF
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized

object ProofToHash {

  def digest(signature: Proofs.Signature.VrfEd25519)(implicit vrf: Ed25519VRF): Rho =
    Sized.strictUnsafe(Bytes(vrf.vrfProofToHash(signature.bytes.data.toArray)))
}
