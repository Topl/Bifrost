package co.topl

import co.topl.attestation.ops

package object attestation {
  trait Implicits extends ops.AttestationBytesOps.ToAttestationBytesOps

  object implicits extends Implicits
}
