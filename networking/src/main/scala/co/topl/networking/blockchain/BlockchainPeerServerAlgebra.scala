package co.topl.networking.blockchain

import co.topl.consensus.models.BlockHeader
import co.topl.node.models.BlockBody
import co.topl.models.{SlotData, Transaction, TypedIdentifier}
import fs2._

/**
 * Serves local blockchain data to a remote peer
 */
trait BlockchainPeerServerAlgebra[F[_]] {
  def localBlockAdoptions: F[Stream[F, TypedIdentifier]]
  def localTransactionNotifications: F[Stream[F, TypedIdentifier]]
  def getLocalSlotData(id:          TypedIdentifier): F[Option[SlotData]]
  def getLocalHeader(id:            TypedIdentifier): F[Option[BlockHeader]]
  def getLocalBody(id:              TypedIdentifier): F[Option[BlockBody]]
  def getLocalTransaction(id:       TypedIdentifier): F[Option[Transaction]]
  def getLocalBlockAtHeight(height: Long): F[Option[TypedIdentifier]]
}
