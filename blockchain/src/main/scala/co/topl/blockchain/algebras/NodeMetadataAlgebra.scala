package co.topl.blockchain.algebras

import co.topl.brambl.models.TransactionId
import co.topl.consensus.models.BlockId

trait NodeMetadataAlgebra[F[_]] {

  def readAppVersion: F[Option[String]]
  def setAppVersion(version: String): F[Unit]

  def readInitTime: F[Option[Long]]
  def setInitTime(timestamp: Long): F[Unit]

  def readStakingRegistrationTransactionId: F[Option[TransactionId]]
  def setStakingRegistrationTransactionId(id: TransactionId): F[Unit]

  def readStakingRegistrationBlockId: F[Option[BlockId]]
  def setStakingRegistrationBlockId(id: BlockId): F[Unit]

}
