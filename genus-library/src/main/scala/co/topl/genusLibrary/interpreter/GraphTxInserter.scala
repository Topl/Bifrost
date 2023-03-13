package co.topl.genusLibrary.interpreter

import cats.effect.kernel.Async
import cats.implicits._
import co.topl.genusLibrary.algebras.TxInserter
import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.model.BlockData

class GraphTxInserter[F[_]: Async]() extends TxInserter[F] {

  // TODO implement tx Inserter
  override def insert(block: BlockData): F[Either[Failure, Unit]] = Either.right[Failure, Unit](()).pure[F]

}
