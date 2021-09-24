package co.topl.minting.algebras

import co.topl.minting.algebras.LeaderElectionMintingAlgebra.VrfHit
import co.topl.models._

/**
 * Staking means participating in the blockchain network.  A staker uses their funds to provide elgibility certificates.
 * A staker also certifies/signs newly minted blocks.
 */
trait StakingAlgebra[F[_]] {

  def address: F[TaktikosAddress]

  def elect(parent: BlockHeaderV2, slot: Slot): F[Option[VrfHit]]

  def certifyBlock(unsigned: BlockV2.Unsigned): F[BlockV2]
}
