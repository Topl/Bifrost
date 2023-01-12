package co.topl.minting.algebras

import co.topl.minting.models.VrfHit
import co.topl.models._

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
    unsignedBlockBuilder: BlockHeader.Unsigned.PartialOperationalCertificate => Block.Unsigned
  ): F[Option[Block]]
}
