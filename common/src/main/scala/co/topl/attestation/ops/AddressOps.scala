package co.topl.attestation.ops

import cats.implicits._
import co.topl.attestation.Address
import co.topl.attestation.ops.EvidenceOps.ToTypedEvidenceFailure
import co.topl.models.{DionAddress, NetworkPrefix}
import co.topl.attestation.ops.EvidenceOps.implicits._

import scala.language.implicitConversions

class AddressOps(val address: Address) extends AnyVal {
  import AddressOps._

  def toDionAddress: Either[ToDionAddressFailure, DionAddress] =
    address.evidence.toTypedEvidence
      .map(evidence => DionAddress(NetworkPrefix(address.networkPrefix), evidence))
      .leftMap(ToDionAddressFailures.InvalidEvidence.apply)
}

object AddressOps {

  sealed trait ToDionAddressFailure

  object ToDionAddressFailures {
    case class InvalidEvidence(inner: ToTypedEvidenceFailure) extends ToDionAddressFailure
  }

  trait ToAddressOps {
    implicit def addressOpsFromAddress(address: Address): AddressOps = new AddressOps(address)
  }

  trait Implicits extends ToAddressOps

  object implicits extends Implicits
}
