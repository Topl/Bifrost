package co.topl.genusLibrary.interpreter.mediator

import cats.data.EitherT
import cats.effect.Async
import cats.implicits._
import co.topl.genusLibrary.algebras.mediator.BodyMediatorAlgebra
import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.model.BlockData
import co.topl.genusLibrary.orientDb.{StoreFacade, VertexFetcher}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class GraphBodyMediator[F[_]: Async](
  storeFacade:   StoreFacade,
  vertexFetcher: VertexFetcher[F]
) extends BodyMediatorAlgebra[F] {

  implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  override def mediate(block: BlockData): F[Either[Failure, Unit]] = {
    val graph = storeFacade.getGraph
    graph.withEffectfulTransaction {
      (
        for {
          headerVertex <- EitherT(vertexFetcher.fetchHeader(block.header))
          bodyVertex   <- EitherT(vertexFetcher.fetchBody(block.body, block.header.height))
          headerAndBodyEdge <- EitherT(
            graph.addEdge(headerVertex, bodyVertex).asRight[Failure].pure[F]
          )
        } yield headerAndBodyEdge
      ).value.map(_.map(_ => ()))
    }
  }

}
