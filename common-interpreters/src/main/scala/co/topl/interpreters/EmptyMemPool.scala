package co.topl.interpreters

import cats.Applicative
import cats.implicits._
import cats.data.Chain
import co.topl.algebras.MemPoolAlgebra
import co.topl.models.{Transaction, TypedIdentifier}

object EmptyMemPool {

  def make[F[_]: Applicative]: F[MemPoolAlgebra[F]] =
    ((_ => Chain.empty[Transaction].pure[F]): MemPoolAlgebra[F]).pure[F]
}
