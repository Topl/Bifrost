package co.topl.attestation.proof

import co.topl.attestation.Proposition

import scala.util.Try

trait ProofEncoder[P <: Proposition, PR <: Proof[P]] {
  def encodeString (proof: Proof[P]): String
}

object ProofEncoder {
  def apply[P <: Proposition, PR <: Proof[P]](implicit ev: ProofEncoder[P, PR]): ProofEncoder[P, PR] = ev
  def instance[P <: Proposition, PR <: Proof[P]](f: Proof[P] => String): ProofEncoder[P, PR] = (proof: Proof[P]) => f(proof)
}


trait ProofDecoder[P <: Proposition, PR <: Proof[P]] {
  def decodeString (proofStr: String): Try[Proof[P]]
}

object ProofDecoder {
  def apply[P <: Proposition, PR <: Proof[P]](implicit ev: ProofDecoder[P, PR]): ProofDecoder[P, PR] = ev
  def instance[P <: Proposition, PR <: Proof[P]](f: String => Try[Proof[P]]): ProofDecoder[P, PR] = (proofStr: String) => f(proofStr)
}
