package co.topl.minting.algebras

import co.topl.catsakka.{Iterative, SourceMatNotUsed}
import co.topl.models.{BlockBodyV2, TypedIdentifier}

/**
 * Assembles an ideal Block Body using the given parent Block ID.
 */
trait BlockPackerAlgebra[F[_]] {

  /**
   * Produce an ideal Block Body
   * @return a Stream that is expected to be eagerly materialized, but the first element request will
   *         be delayed.  Only one element is expected to be pulled from the returned Source.
   */
  def improvePackedBlock(parentBlockId: TypedIdentifier): F[Iterative[F, BlockBodyV2.Full, BlockBodyV2.Full]]
}
