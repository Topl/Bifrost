package co.topl.consensus

import cats.Eq
import cats.implicits._
import co.topl.crypto.signing.Ed25519VRF
import co.topl.models.{BlockHeaderV2, Eta, Rho, SlotId}
import co.topl.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._

/**
 * Represents _some_ of the data of a Block Header.  The subset of represented data is primarily used for chain
 * selection, eta, and VRF/eligibility verification.  A Block Header includes an operational certificate which
 * unnecessarily consumes memory.
 */
case class SlotData(slotId: SlotId, parentSlotId: SlotId, rho: Rho, eta: Eta, height: Long)

object SlotData {

  def apply(blockHeaderV2: BlockHeaderV2)(implicit ed25519VRF: Ed25519VRF): SlotData =
    SlotData(
      blockHeaderV2.slotId,
      blockHeaderV2.parentSlotId,
      ed25519VRF.proofToHash(blockHeaderV2.eligibilityCertificate.vrfSig),
      blockHeaderV2.eligibilityCertificate.eta,
      blockHeaderV2.height
    )

  implicit val slotDataEq: Eq[SlotData] =
    (a, b) =>
      a.slotId === b.slotId &&
      a.parentSlotId === b.parentSlotId &&
      a.rho === b.rho &&
      a.eta === b.eta &&
      a.height === b.height
}
