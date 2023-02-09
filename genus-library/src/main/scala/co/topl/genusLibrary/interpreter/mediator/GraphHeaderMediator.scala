package co.topl.genusLibrary.interpreter.mediator

import cats.data.EitherT
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.genusLibrary.algebras.mediator.HeaderMediatorAlgebra
import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.model.BlockData
import co.topl.genusLibrary.orientDb.{StoreFacade, VertexFetcher}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/**
 * Implementation of the Header Mediator. After each element insertion, the mediation should take place on the Store.
 * For more information, refer to the HeaderMediatorAlgebra trait.
 * @param storeFacade Facade to interact with the store
 * @param vertexFetcher Utils to retrieve different block vertices
 * @tparam F the effect-ful context to retrieve the value in
 */
class GraphHeaderMediator[F[_]: Async](
  storeFacade:   StoreFacade,
  vertexFetcher: VertexFetcher[F]
) extends HeaderMediatorAlgebra[F] {

  implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  override def mediate(block: BlockData): F[Either[Failure, Unit]] = {
    val graph = storeFacade.getGraph
    graph.withEffectfulTransaction {
      (
        for {
          currentHeaderVertex  <- EitherT(vertexFetcher.fetchHeader(block.header))
          previousHeaderVertex <- EitherT(vertexFetcher.fetchPreviousHeader(block.header))
          headersEdge <- EitherT(
            graph.addEdge(previousHeaderVertex, currentHeaderVertex).asRight[Failure].pure[F]
          )
        } yield headersEdge
      ).value.map(_.map(_ => ()))
    }
  }

}
