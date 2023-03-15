package co.topl.genusLibrary.algebras.mediator

import co.topl.genus.services.Txo
import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.model.BlockData

/**
 * Mediator of transaction outputs inserted to a data store. Based on the Mediator software design pattern.
 * For more information, refer to <a href="https://en.wikipedia.org/wiki/Mediator_pattern">Mediator pattern</a>
 *
 * @tparam F the effect-ful context to retrieve the value in
 */
trait TransactionOutputMediatorAlgebra[F[_]] {

  /**
   * Mediate after insertion of a transaction output.
   *
   * @param block the full block data for context reasons
   * @return Unit
   */
  def mediate(txo: Txo, block: BlockData): F[Either[Failure, Unit]]

}
