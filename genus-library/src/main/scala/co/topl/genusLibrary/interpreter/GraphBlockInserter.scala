package co.topl.genusLibrary.interpreter

import cats.effect.kernel.Async
import cats.implicits._
import co.topl.genusLibrary.algebras.{BlockInserterAlgebra, BodyInserter, HeaderInserter, TxInserter}
import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.model.BlockData

class GraphBlockInserter[F[_]: Async](
  headerInserter: HeaderInserter[F],
  bodyInserter:   BodyInserter[F],
  txInserter:     TxInserter[F]
) extends BlockInserterAlgebra[F] {

  override def insert(block: BlockData): F[Either[Failure, Unit]] = for {
    txInsertionResult   <- headerInserter.insert(block) // TODO testing header Inserter,
//    _                 <- bodyInserter.insert(block)
//    txInsertionResult <- txInserter.insert(block)
  } yield txInsertionResult

}
