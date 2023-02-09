package co.topl.minting.algebras

import co.topl.minting.models.VrfHit
import co.topl.models._

/**
 * Staking means participating in the blockchain network.  A staker uses their funds to provide elgibility certificates.
 * A staker also certifies/signs newly minted blocks.
 */
trait StakingAlgebra[F[_]] {

  /**
   * TODO add documentation https://topl.atlassian.net/browse/BN-845
   * @return
   */
  def address: F[StakingAddresses.Operator]

  /**
   * TODO add documentation https://topl.atlassian.net/browse/BN-845
   * @param parentSlotId
   * @param slot
   * @return
   */
  def elect(parentSlotId: SlotId, slot: Slot): F[Option[VrfHit]]

  /**
   * TODO add documentation https://topl.atlassian.net/browse/BN-845
   * @param parentSlotId
   * @param slot
   * @param unsignedBlockBuilder
   * @return
   */
  def certifyBlock(
    parentSlotId:         SlotId,
    slot:                 Slot,
    unsignedBlockBuilder: BlockHeader.Unsigned.PartialOperationalCertificate => Block.Unsigned
  ): F[Option[Block]]
}
