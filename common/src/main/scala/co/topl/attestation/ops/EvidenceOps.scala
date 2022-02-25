package co.topl.attestation.ops

import cats.implicits._
import co.topl.attestation.{Evidence => DionEvidence}
import co.topl.models.{Bytes, TypedEvidence}
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Lengths, Sized}

import scala.language.implicitConversions

class EvidenceOps(val evidence: DionEvidence) extends AnyVal {
  import EvidenceOps._

  def toTypedEvidence: Either[ToTypedEvidenceFailure, TypedEvidence] =
    for {
      typePrefix <- evidence.evBytes.headOption.toRight(ToTypedEvidenceFailures.EmptyEvidence)
      data <- Sized
        .strict[Bytes, Lengths.`32`.type](Bytes(evidence.evBytes.tail))
        .leftMap(ToTypedEvidenceFailures.InvalidEvidenceSize.apply)
    } yield TypedEvidence(typePrefix, data)
}

object EvidenceOps {

  sealed trait ToTypedEvidenceFailure

  object ToTypedEvidenceFailures {
    case class InvalidEvidenceSize(inner: Sized.InvalidLength) extends ToTypedEvidenceFailure
    case object EmptyEvidence extends ToTypedEvidenceFailure
  }

  trait ToEvidenceOps {
    implicit def evidenceOpsFromEvidence(evidence: DionEvidence): EvidenceOps = new EvidenceOps(evidence)
  }

  trait Implicits extends ToEvidenceOps

  object implicits extends Implicits
}
