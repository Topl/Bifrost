package co.topl.algebras

import cats.data.Chain
import co.topl.models.{Transaction, TypedIdentifier}

trait MemPoolAlgebra[F[_]] {
  def unappliedTransactionsAt(blockId: TypedIdentifier): F[Chain[Transaction]]
}
