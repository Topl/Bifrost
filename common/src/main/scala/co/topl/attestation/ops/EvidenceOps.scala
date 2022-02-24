package co.topl.attestation.ops

import co.topl.attestation.{Evidence => DionEvidence}
import co.topl.models.TypedEvidence
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Sized
import scodec.bits.ByteVector

import scala.language.implicitConversions

class EvidenceOps(val evidence: DionEvidence) extends AnyVal {

  def upgrade: TypedEvidence =
    TypedEvidence(evidence.evBytes.head, Sized.strictUnsafe(ByteVector(evidence.evBytes.tail)))
}

object EvidenceOps {

  trait ToEvidenceOps {
    implicit def evidenceOpsFromEvidence(evidence: DionEvidence): EvidenceOps = new EvidenceOps(evidence)
  }

  trait Implicits extends ToEvidenceOps

  object implicits extends Implicits
}
