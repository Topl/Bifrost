package co.topl.attestation.ops

import co.topl.attestation.{Address, Evidence}
import co.topl.models.DionAddress

import scala.language.implicitConversions

class DionAddressOps(private val value: DionAddress) extends AnyVal {
  def toAddress: Address = Address(Evidence(value.typedEvidence.allBytes.toArray))(value.networkPrefix.value)
}

object DionAddressOps {

  trait ToDionAddressOps {
    implicit def dionAddressOpsFromDionAddress(value: DionAddress): DionAddressOps = new DionAddressOps(value)
  }

  trait Implicits extends ToDionAddressOps

  object implicits extends Implicits
}
