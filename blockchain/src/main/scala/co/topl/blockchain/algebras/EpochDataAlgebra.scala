package co.topl.blockchain.algebras

import co.topl.models.Epoch
import quivr.models.Int128

/**
 * Provides epoch-level statistics
 */
trait EpochDataAlgebra[F[_]] {

  /**
   * Constructs the EpochData for the requested epoch.  The "current" epoch is updated as blocks are adopted.
   * @param epoch the epoch number to request
   * @return EpochData
   */
  def dataOf(epoch: Epoch): F[Option[EpochData]]

}

// TODO: Re-implement in Protobuf in BN-1046
case class EpochData(
  epoch:                  Epoch,
  eon:                    Long,
  era:                    Long,
  isComplete:             Boolean,
  startHeight:            Long,
  endHeight:              Long,
  startSlot:              Long,
  endSlot:                Long,
  startTimestamp:         Long,
  endTimestamp:           Long,
  transactionCount:       Long,
  totalTransactionReward: Int128,
  activeStake:            Int128,
  inactiveStake:          Int128,
  dataBytes:              Long
)
