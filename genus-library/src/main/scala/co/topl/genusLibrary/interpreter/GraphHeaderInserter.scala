package co.topl.genusLibrary.interpreter

import cats.data.EitherT
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.genusLibrary.algebras.{HeaderInserter, Mediator}
import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.model.BlockData
import co.topl.genusLibrary.orientDb.StoreFacade
import co.topl.genusLibrary.orientDb.GenusGraphMetadata._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class GraphHeaderInserter[F[_]: Async](
  orientDB: StoreFacade,
  mediator: Mediator[F]
) extends HeaderInserter[F] {

  implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  override def insert(block: BlockData): F[Either[Failure, Unit]] = {
    val graph = orientDB.getGraph[F]

    (for {
      _ <- EitherT(graph.withEffectfulTransaction {
        graph.createVertex(block.header).asRight[Failure].pure[F]
      })
      mediation <- EitherT(mediator.afterHeaderInserted(block))
    } yield mediation).value
  }

}
