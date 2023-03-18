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
    // TODO: insert block header is working, but when we implement body and tx insertion, we need to handle the DB trasaction at this level, and no inside each operation
    txInsertionResult <- headerInserter.insert(block)
//    _                 <- bodyInserter.insert(block) // TODO fix body inserts
//    txInsertionResult <- txInserter.insert(block) // TODO fix transaction inserts
  } yield txInsertionResult

}
