package co.topl.attestation.ops

import cats.implicits._
import co.topl.attestation.Address
import co.topl.attestation.ops.EvidenceOps.ToTypedEvidenceFailure
import co.topl.models.SpendingAddress
import co.topl.attestation.ops.EvidenceOps.implicits._

import scala.language.implicitConversions

/**
 * Extension operations for [[Address]] values.
 * @param address the value to have extension methods operate on
 */
class AddressOps(private val address: Address) extends AnyVal {
  import AddressOps._

  /**
   * Attempts to convert the address to an equivalent [[SpendingAddress]] value.
   *
   * @return if successful, a [[SpendingAddress]], otherwise a [[ToDionAddressFailure]] represnting an error with the
   *         conversion
   */
  def toSpendingAddress: Either[ToDionAddressFailure, SpendingAddress] =
    address.evidence.toTypedEvidence
      .map(evidence => SpendingAddress(evidence))
      .leftMap(ToSpendingAddressFailures.InvalidEvidence.apply)
}

object AddressOps {

  sealed trait ToDionAddressFailure

  object ToSpendingAddressFailures {
    case class InvalidEvidence(inner: ToTypedEvidenceFailure) extends ToDionAddressFailure
  }

  trait ToAddressOps {
    implicit def addressOpsFromAddress(address: Address): AddressOps = new AddressOps(address)
  }

  trait Implicits extends ToAddressOps

  object implicits extends Implicits
}
