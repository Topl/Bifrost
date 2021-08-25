package co.topl.modifier.transaction.builder.polyTransfer

import co.topl.attestation.{EvidenceProducer, Proposition}
import co.topl.modifier.transaction.PolyTransfer
import co.topl.utils.Identifiable
import simulacrum.typeclass

@typeclass
trait PolyTransferBuildStrategy[A] {

  def execute[P <: Proposition: EvidenceProducer: Identifiable](
    strategy: A
  ): Either[InvalidPolyTransfer, PolyTransfer[P]]
}
