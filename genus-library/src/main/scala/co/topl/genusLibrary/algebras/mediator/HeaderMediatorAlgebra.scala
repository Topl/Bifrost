package co.topl.genusLibrary.algebras.mediator

import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.model.BlockData

/**
 * Mediator of headers inserted to a data store. Based on the Mediator software design pattern.
 * For more information, refer to <a href="https://en.wikipedia.org/wiki/Mediator_pattern">Mediator pattern</a>
 *
 * @tparam F the effect-ful context to retrieve the value in
 */
trait HeaderMediatorAlgebra[F[_]] {

  /**
   * Mediate after insertion of a header.
   *
   * @param block the full block data for context reasons
   * @return Unit
   */
  def mediate(block: BlockData): F[Either[Failure, Unit]]

}
