package co.topl.networking.blockchain

import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.BlockId
import co.topl.consensus.models.SlotData
import co.topl.node.models.BlockBody
import fs2._

/**
 * Serves local blockchain data to a remote peer
 */
trait BlockchainPeerServerAlgebra[F[_]] {
  def localBlockAdoptions: F[Stream[F, BlockId]]
  def localTransactionNotifications: F[Stream[F, TransactionId]]
  def getLocalSlotData(id:          BlockId): F[Option[SlotData]]
  def getLocalHeader(id:            BlockId): F[Option[BlockHeader]]
  def getLocalBody(id:              BlockId): F[Option[BlockBody]]
  def getLocalTransaction(id:       TransactionId): F[Option[IoTransaction]]
  def getLocalBlockAtHeight(height: Long): F[Option[BlockId]]
}
