package co.topl.ledger.algebras

import co.topl.{models => legacyModels}
import legacyModels.{Box, TypedIdentifier}

trait BoxStateAlgebra[F[_]] {

  /**
   * Indicates if a particular box is spendable at the given block ID
   * @return F[true] if the box exists and is spendable
   *         F[false] if the box either never existed or has been spent already.  The interpretation should make no
   *         distinction between the two scenarios.
   */
  def boxExistsAt(blockId: TypedIdentifier)(boxId: Box.Id): F[Boolean]
}
