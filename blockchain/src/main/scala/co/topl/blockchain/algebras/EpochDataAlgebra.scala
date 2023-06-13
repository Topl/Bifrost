package co.topl.blockchain.algebras

import co.topl.models.Epoch
import co.topl.proto.node.EpochData

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
