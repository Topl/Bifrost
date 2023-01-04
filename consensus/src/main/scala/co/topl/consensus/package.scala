package co.topl

import cats.Order
import co.topl.models._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.crypto.signing.Ed25519VRF
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Sized

package object consensus {

  implicit class OrderSupport[T](order: Order[T]) {

    def tiebreakWith(other: Order[T]): Order[T] =
      Order.whenEqual(order, other)
  }

  implicit class BlockHeaderOps(blockHeader: BlockHeader) {

    import co.topl.codecs.bytes.tetra.TetraIdentifiableInstances._
    import co.topl.typeclasses.implicits._

    def slotData(implicit ed25519VRF: Ed25519VRF): SlotData =
      SlotData(
        SlotId(blockHeader.slot, blockHeader.id),
        blockHeader.parentSlotId,
        Rho(Sized.strictUnsafe(ed25519VRF.proofToHash(blockHeader.eligibilityCertificate.vrfSig.bytes.data))),
        blockHeader.eligibilityCertificate.eta,
        blockHeader.height
      )
  }
}
