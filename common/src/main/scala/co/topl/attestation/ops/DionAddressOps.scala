package co.topl.attestation.ops

import co.topl.attestation.Evidence.EvidenceContent
import co.topl.attestation.{Address, Evidence}
import co.topl.models.DionAddress

import scala.language.implicitConversions

class DionAddressOps(private val value: DionAddress) extends AnyVal {

  def toAddress: Address =
    Address(
      Evidence(value.typedEvidence.typePrefix, EvidenceContent(value.typedEvidence.evidence.data.toArray))
    )(
      value.networkPrefix.value
    )
}

object DionAddressOps {

  trait ToDionAddressOps {

    implicit def dionAddressOpsFromValue(value: DionAddress): DionAddressOps =
      new DionAddressOps(value)
  }

  trait Implicits extends ToDionAddressOps

  object implicits extends Implicits
}
