package co.topl.genusLibrary.algebras

import co.topl.brambl.models.Identifier.IoTransaction32
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.genus.services.TransactionReceipt
import co.topl.genusLibrary.model.GE

/**
 * Algebra which defines fetch operations of transactions against the stored Ledger.
 *
 * @tparam F the effect-ful context to retrieve the value in
 */
trait TransactionFetcherAlgebra[F[_]] {

  /**
   * Fetch a Transaction on the stored Ledger
   * @param ioTransaction32  Transaction Identifier filter by field
   * @return Optional Transaction, None if it was not found
   */
  def fetchTransaction(ioTransaction32: IoTransaction32): F[Either[GE, Option[IoTransaction]]]

  /**
   * Fetch a Transaction TransactionReceipt (includes ioTx, blockId, chain distance,...) on the stored Ledger
   *
   * @param ioTransaction32 Transaction Identifier filter by field
   * @return Optional Transaction, None if it was not found
   */
  def fetchTransactionReceipt(ioTransaction32: IoTransaction32): F[Either[GE, Option[TransactionReceipt]]]

}
