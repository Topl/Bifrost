package co.topl.genusLibrary.interpreter

import cats.data.EitherT
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.genusLibrary.algebras.mediator.BodyMediatorAlgebra
import co.topl.genusLibrary.algebras.BodyInserter
import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.model.BlockData
import co.topl.genusLibrary.orientDb.GenusGraphMetadata._
import co.topl.genusLibrary.orientDb.StoreFacade
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class GraphBodyInserter[F[_]: Async](
  orientDB:     StoreFacade,
  bodyMediator: BodyMediatorAlgebra[F]
) extends BodyInserter[F] {

  implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  override def insert(block: BlockData): F[Either[Failure, Unit]] = {
    val graph = orientDB.getGraph[F]

    (for {
      _ <- EitherT(graph.withEffectfulTransaction {
        graph.createVertex(block.body).asRight[Failure].pure[F]
      })
      mediation <- EitherT(bodyMediator.mediate(block))
    } yield mediation).value
  }

}
