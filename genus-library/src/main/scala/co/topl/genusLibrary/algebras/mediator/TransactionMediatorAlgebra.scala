package co.topl.genusLibrary.algebras.mediator

import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.model.BlockData
import co.topl.models.Transaction

/**
 * Mediator of transactions inserted to a data store. Based on the Mediator software design pattern.
 * For more information, refer to <a href="https://en.wikipedia.org/wiki/Mediator_pattern">Mediator pattern</a>
 *
 * @tparam F the effect-ful context to retrieve the value in
 */
trait TransactionMediatorAlgebra[F[_]] {

  /**
   * Mediate after insertion of a transaction.
   *
   * @param transaction inserted transaction
   * @param block the full block data for context reasons
   * @return Unit
   */
  def mediate(transaction: Transaction, block: BlockData): F[Either[Failure, Unit]]

}
