package co.topl.attestation

package object ops {
  trait Implicits extends AttestationBytesOps.ToAttestationBytesOps with AddressOps.Implicits with EvidenceOps.Implicits

  object implicits extends Implicits
}
