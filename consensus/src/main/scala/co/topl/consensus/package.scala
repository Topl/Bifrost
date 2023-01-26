package co.topl

import co.topl.models._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.crypto.signing.Ed25519VRF
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Sized
import scodec.bits.ByteVector

package object consensus {

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

  /**
   * TODO
   * @param blockHeader helper for SlotData
   */
  implicit class ConsensusBlockHeaderOps(blockHeader: co.topl.consensus.models.BlockHeader) {

    import co.topl.codecs.bytes.tetra.TetraIdentifiableInstances._
    import co.topl.typeclasses.implicits._

    def slotData(implicit ed25519VRF: Ed25519VRF): SlotData =
      SlotData(
        SlotId(blockHeader.slot, blockHeader.id),
        SlotId(blockHeader.parentSlot, TypedBytes.headerFromBlockId(blockHeader.parentHeaderId)),
        Rho(
          Sized.strictUnsafe(
            ed25519VRF.proofToHash(
              blockHeader.eligibilityCertificate
                .map(_.vrfSig.toByteArray)
                .map(ByteVector(_))
                .getOrElse(ByteVector.empty)
            )
          )
        ),
        Sized.strictUnsafe(
          blockHeader.eligibilityCertificate
            .map(_.eta.toByteArray)
            .map(ByteVector(_))
            .getOrElse(ByteVector.empty)
        ),
        blockHeader.height
      )
  }
}
