package co.topl.genusLibrary.orientDb

import cats.effect.kernel.Async
import cats.implicits._
import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.orientDb.wrapper.{GraphTxWrapper, WrappedEdge, WrappedVertex}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.util.{Success, Try}

/**
 * Even though Tinkerpop's API is very high level and great in design for a Java application,
 * there are some use cases that feel a bit low level on our end, may induce duplicate code and/or bad practices.
 * @param wrappedGraph Wrapped Tinkerpop's implementation of an TX Orient Graph.
 */
class GraphTxDAO[F[_]: Async](wrappedGraph: GraphTxWrapper) {

  implicit private val logger: Logger[F] =
    Slf4jLogger.getLoggerFromClass[F](this.getClass)

  /**
   * Commits graph at the end of a function process if it ran successfully
   *
   * @param transactionalFun function process wrapped in an effect-ful context that should be run inside a transaction
   */
  def withEffectfulTransaction[T <: Any](transactionalFun: => F[Either[Failure, T]]): F[Unit] =
    transactionalFun flatMap withTransaction

  /**
   * Vertex creator under the given graph on instantiation
   *
   * @param elem element to insert
   * @param schema schema for given element to be inserted
   * @tparam T abstract type of the given element and type parameter of given schema
   * @return element and created vertex
   */
  def createVertex[T](
    elem: T
  )(implicit
    schema: VertexSchema[T]
  ): (T, WrappedVertex) =
    updateVertex(elem, wrappedGraph.addVertex(s"class:${schema.name}"))

  /**
   * Vertex updater from an element
   *
   * @param elem element to update
   * @param vertex vertex to update
   * @param schema schema for given element to be updated
   * @tparam T abstract type of the given element and type parameter of given schema
   * @return element and updated vertex
   */
  def updateVertex[T](
    elem:   T,
    vertex: WrappedVertex
  )(implicit
    schema: VertexSchema[T]
  ): (T, WrappedVertex) = {
    schema
      .encode(elem)
      .foreach { case (key, value) =>
        vertex.setProperty(key, value)
      }

    (elem, vertex)
  }

  /**
   * Edge creator. Creates edge from outVertex to inVertex.
   * outVertex -[label]-> inVertex
   *
   * @param outVertex vertex from which the edge starts
   * @param inVertex  vertex from which the edge ends
   * @param label     description of the edge
   * @return created edge
   */
  def addEdge(
    outVertex: WrappedVertex,
    inVertex:  WrappedVertex,
    label:     Option[String] = None
  ): WrappedEdge =
    wrappedGraph.addEdge(null, outVertex, inVertex, label.orNull)

  private def withTransaction[T <: Any](transactionalFun: Either[Failure, T]): F[Unit] =
    transactionalFun match {
      case Left(ex) => Logger[F].error(s"Something went wrong with the transaction function, won't commit. Error=[$ex]")
      case Right(_) =>
        Logger[F]
          .info("Committing transaction")
          .as(Try(wrappedGraph.commit()))
          .map {
            case util.Failure(ex) =>
              // TODO Specify for each exception type what we should do.
              // Ex: OConcurrentModificationException, ORecordDuplicatedException
              Logger[F]
                .error(s"Something went wrong while trying to commit transaction. Error=[$ex]")
                .as(wrappedGraph.rollback())
            case Success(_) =>
              Logger[F].info("Transaction committed")
          }
    }

}
