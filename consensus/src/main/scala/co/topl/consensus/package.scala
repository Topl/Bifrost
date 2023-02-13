package co.topl

import co.topl.models._
import co.topl.models.utility._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.crypto.signing.Ed25519VRF
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.consensus.{models => consensusModels}

package object consensus {

  implicit class BlockHeaderOps(blockHeader: BlockHeader) {

    import co.topl.codecs.bytes.tetra.TetraIdentifiableInstances._
    import co.topl.typeclasses.implicits._

    // TODO Remove after full model replacement
    def slotData(implicit ed25519VRF: Ed25519VRF): SlotDataLegacy =
      SlotDataLegacy(
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
  implicit class ConsensusBlockHeaderOps(blockHeader: consensusModels.BlockHeader) {

    import co.topl.codecs.bytes.tetra.TetraIdentifiableInstances._
    import co.topl.typeclasses.implicits._

    def slotData(implicit ed25519VRF: Ed25519VRF): SlotDataLegacy =
      SlotDataLegacy(
        SlotId(blockHeader.slot, blockHeader.id),
        SlotId(blockHeader.parentSlot, blockHeader.parentHeaderId),
        Rho(
          Sized.strictUnsafe(
            ed25519VRF.proofToHash(
              blockHeader.eligibilityCertificate.vrfSig.value
            )
          )
        ),
        Sized.strictUnsafe(blockHeader.eligibilityCertificate.eta),
        blockHeader.height
      )
  }
}
