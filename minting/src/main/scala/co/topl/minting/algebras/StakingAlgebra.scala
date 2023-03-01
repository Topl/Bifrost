package co.topl.minting.algebras

import co.topl.minting.models.VrfHit
import co.topl.models._
import co.topl.consensus.models.SlotId
import co.topl.models.utility.Ratio
import co.topl.node.models.Block

/**
 * Staking means participating in the blockchain network.  A staker uses their funds to provide elgibility certificates.
 * A staker also certifies/signs newly minted blocks.
 */
trait StakingAlgebra[F[_]] {

  def address: F[StakingAddresses.Operator]

  def elect(parentSlotId: SlotId, slot: Slot): F[Option[VrfHit]]

  def certifyBlock(
    parentSlotId:         SlotId,
    slot:                 Slot,
    unsignedBlockBuilder: BlockHeader.Unsigned.PartialOperationalCertificate => co.topl.models.Block.Unsigned
  ): F[Option[Block]]

  def getHit(
    relativeStake: Ratio,
    slot:          Slot,
    slotDiff:      Long,
    eta:           Eta
  ): F[Option[VrfHit]]
}
