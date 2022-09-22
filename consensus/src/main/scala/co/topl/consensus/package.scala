package co.topl

import cats.Order
import co.topl.crypto.signing.Ed25519VRF
import co.topl.models.{BlockHeaderV2, SlotData}

package object consensus {

  implicit class OrderSupport[T](order: Order[T]) {

    def tiebreakWith(other: Order[T]): Order[T] =
      Order.whenEqual(order, other)
  }

  implicit class BlockHeaderV2Ops(blockHeaderV2: BlockHeaderV2) {

    import co.topl.codecs.bytes.tetra.TetraIdentifiableInstances._
    import co.topl.typeclasses.implicits._

    def slotData(implicit ed25519VRF: Ed25519VRF): SlotData =
      SlotData(
        blockHeaderV2.slotId,
        blockHeaderV2.parentSlotId,
        ed25519VRF.proofToHash(blockHeaderV2.eligibilityCertificate.vrfSig),
        blockHeaderV2.eligibilityCertificate.eta,
        blockHeaderV2.height
      )
  }
}
