package co.topl.consensus.vrf

import co.topl.crypto.signatures.Ed25519VRF
import co.topl.models._
import co.topl.models.utility.{Lengths, Sized}
import co.topl.models.utility.Lengths._
import co.topl.models.utility.HasLength.instances._

object ProofToHash {
  val vrf = new Ed25519VRF
  vrf.precompute()

  //64-byte output
  def digest(signature: Proofs.Consensus.VrfTest): Rho =
    Sized.strict[Bytes, Lengths.`64`.type](Bytes(vrf.vrfProofToHash(signature.bytes.data.toArray))).toOption.get

  def digest(signature: Proofs.Consensus.Nonce): Rho =
    Sized.strict[Bytes, Lengths.`64`.type](Bytes(vrf.vrfProofToHash(signature.bytes.data.toArray))).toOption.get
}
