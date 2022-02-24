package co.topl.attestation.ops

import co.topl.attestation.Address
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.{DionAddress, NetworkPrefix, TypedEvidence}
import co.topl.models.utility.Sized
import scodec.bits.ByteVector

import scala.language.implicitConversions

class AddressOps(val address: Address) extends AnyVal {

  def upgrade: DionAddress = DionAddress(
    NetworkPrefix(address.networkPrefix),
    TypedEvidence(address.evidence.evBytes.head, Sized.strictUnsafe(ByteVector(address.evidence.evBytes.tail)))
  )
}

object AddressOps {

  trait ToAddressOps {
    implicit def addressOpsFromAddress(address: Address): AddressOps = new AddressOps(address)
  }

  trait Implicits extends ToAddressOps

  object implicits extends Implicits
}
