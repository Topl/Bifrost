package co.topl.minting.algebras

import co.topl.consensus.models.BlockHeader
import co.topl.minting.models.VrfHit
import co.topl.models._
import co.topl.consensus.models.SlotId
import co.topl.consensus.models.StakingAddress

/**
 * Staking means participating in the blockchain network.  A staker uses their funds to provide elgibility certificates.
 * A staker also certifies/signs newly minted blocks.
 */
trait StakingAlgebra[F[_]] {

  /**
   * The staker's staking (account) address, usually a verification key
   */
  def address: F[StakingAddress]

  /**
   * Attempt to form a VrfHit for the given parent+slot combination
   * @param parentSlotId The slotId of the parent block
   * @param slot The current/test/global slot
   * @return Some(hit) if eligible, None if ineligible
   */
  def elect(parentSlotId: SlotId, slot: Slot): F[Option[VrfHit]]

  /**
   * Constructs a (partial) operational certificate, applies it to the given block builder function, and forms
   * the final proof/signature of the BlockHeader
   * @param parentSlotId The parent of the new block
   * @param slot The slot of the new block
   * @param unsignedBlockBuilder a function that constructs an unsigned BlockHeader from the given
   *                             partial operational certificate
   * @return Some(BlockHeader) in most cases.  But in certain edge cases such as a node restart, the staker may be
   *         temporarily unable to certify blocks.
   */
  def certifyBlock(
    parentSlotId:         SlotId,
    slot:                 Slot,
    unsignedBlockBuilder: UnsignedBlockHeader.PartialOperationalCertificate => UnsignedBlockHeader
  ): F[Option[BlockHeader]]
}
