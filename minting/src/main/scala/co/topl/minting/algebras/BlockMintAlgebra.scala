package co.topl.minting.algebras

import co.topl.models.{BlockHeaderV2, BlockV2, Slot, Transaction}

/**
 * Minting is the act of _attempting_ to create a new Block at some Slot to be appended to a blockchain.
 * A Mint may be unable to mint a block at a particular slot, in which case it returns None and skips
 * the attempt.
 */
trait BlockMintAlgebra[F[_]] {
  def mint(parent: BlockHeaderV2, transactions: Seq[Transaction], slot: Slot): F[Option[BlockV2]]
}
