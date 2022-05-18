package co.topl.attestation.ops

import co.topl.attestation.Evidence.EvidenceContent
import co.topl.attestation.{Address, Evidence}
import co.topl.models.FullAddress

import scala.language.implicitConversions

class DionAddressOps(private val value: FullAddress) extends AnyVal {

  def toAddress: Address =
    Address(
      Evidence(
        value.spendingAddress.typedEvidence.typePrefix,
        EvidenceContent(value.spendingAddress.typedEvidence.evidence.data.toArray)
      )
    )(
      value.networkPrefix.value
    )
}

object DionAddressOps {

  trait ToDionAddressOps {

    implicit def dionAddressOpsFromValue(value: FullAddress): DionAddressOps =
      new DionAddressOps(value)
  }

  trait Implicits extends ToDionAddressOps

  object implicits extends Implicits
}
