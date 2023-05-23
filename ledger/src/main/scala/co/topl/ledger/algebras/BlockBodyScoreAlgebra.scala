package co.topl.ledger.algebras

import co.topl.brambl.models.transaction.IoTransaction

/**
 * Assists in determining the risk/reward tradeoff of a given block body (transaction set) candidate
 */
trait BlockBodyScoreAlgebra[F[_]] {

  /**
   * Determines the aggregate "score" of the given candidate set of transactions.
   * @param candidate A collection of Transactions that may be used in a block body
   * @return a number, with higher values indicating a better score
   */
  def scoreOf(candidate: Seq[IoTransaction]): F[BigInt]

}
