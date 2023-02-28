package co.topl.genusLibrary.interpreter.mediator

import cats.effect.IO
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.consensus.models.BlockHeader
import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.model.BlockData
import co.topl.genusLibrary.orientDb.wrapper.{WrappedEdge, WrappedVertex}
import co.topl.genusLibrary.orientDb.{GraphTxDAO, StoreFacade, VertexFetcher}
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.models.generators.node.ModelGenerators.arbitraryNodeBody
import co.topl.node.models.BlockBody
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class GraphBodyMediatorSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  private class GraphTxDAOMock extends GraphTxDAO[F](null)

  private trait VertexFetcherMock extends VertexFetcher[F]

  private val dbFacade = mock[StoreFacade]
  private val vertexFetcher = mock[VertexFetcherMock]

  private val bodyMediator = new GraphBodyMediator[F](dbFacade, vertexFetcher)

  test("On no current header vertex failure, said failure should be returned") {

    PropF.forAllF {
      (
        header: BlockHeader
      ) =>
        withMock {
          val graphTxDao = mock[GraphTxDAOMock]

          val failure = mock[Failure]

          (dbFacade
            .getGraph[F](_: Async[F], _: Logger[F]))
            .expects(*, *)
            .returns(graphTxDao)
            .once()

          (vertexFetcher.fetchHeader _)
            .expects(header)
            .returns(failure.asLeft[WrappedVertex].pure[F])
            .once()

          (graphTxDao.withEffectfulTransaction[Unit] _)
            .expects(*)
            .onCall((f: F[Either[Failure, Unit]]) =>
              f.ensure(new IllegalArgumentException())(_ == failure.asLeft[Unit])
            )
            .once()

          assertIO(
            bodyMediator.mediate(BlockData(header = header, body = null, transactions = null)),
            failure.asLeft[Unit]
          )
        }
    }

  }

  test("On no body vertex failure, said failure should be returned") {

    PropF.forAllF {
      (
        header: BlockHeader,
        body:   BlockBody
      ) =>
        withMock {
          val graphTxDao = mock[GraphTxDAOMock]
          val currentHeaderVertex = mock[WrappedVertex]

          val failure = mock[Failure]

          (dbFacade
            .getGraph[F](_: Async[F], _: Logger[F]))
            .expects(*, *)
            .returns(graphTxDao)
            .once()

          (vertexFetcher.fetchHeader _)
            .expects(header)
            .returns(currentHeaderVertex.asRight[Failure].pure[F])
            .once()

          (vertexFetcher.fetchBody _)
            .expects(body, header.height)
            .returns(failure.asLeft[WrappedVertex].pure[F])
            .once()

          (graphTxDao.withEffectfulTransaction[Unit] _)
            .expects(*)
            .onCall((f: F[Either[Failure, Unit]]) =>
              f.ensure(new IllegalArgumentException())(_ == failure.asLeft[Unit])
            )
            .once()

          assertIO(
            bodyMediator.mediate(BlockData(header = header, body = body, transactions = null)),
            failure.asLeft[Unit]
          )
        }
    }

  }

  test(
    "On header and body vertices, " +
    "on the edge created successfully but with transaction failure, it should be bubbled up"
  ) {

    PropF.forAllF {
      (
        header: BlockHeader,
        body:   BlockBody
      ) =>
        withMock {
          val graphTxDao = mock[GraphTxDAOMock]
          val currentHeaderVertex = mock[WrappedVertex]
          val bodyVertex = mock[WrappedVertex]
          val headerAndBodyEdge = mock[WrappedEdge]

          val leftFailure = mock[Failure].asLeft[Unit]

          (dbFacade
            .getGraph[F](_: Async[F], _: Logger[F]))
            .expects(*, *)
            .returns(graphTxDao)
            .once()

          (vertexFetcher.fetchHeader _)
            .expects(header)
            .returns(currentHeaderVertex.asRight[Failure].pure[F])
            .once()

          (vertexFetcher.fetchBody _)
            .expects(body, header.height)
            .returns(bodyVertex.asRight[Failure].pure[F])
            .once()

          (graphTxDao
            .addEdge(_: WrappedVertex, _: WrappedVertex, _: Option[String]))
            .expects(currentHeaderVertex, bodyVertex, None)
            .returns(headerAndBodyEdge)
            .once()

          (graphTxDao.withEffectfulTransaction[Unit] _)
            .expects(*)
            .onCall((f: F[Either[Failure, Unit]]) =>
              f.ensure(new IllegalArgumentException())(_ == ().asRight[Failure])
                .map(_ => leftFailure)
            )
            .once()

          assertIO(
            bodyMediator.mediate(BlockData(header = header, body = body, transactions = null)),
            leftFailure
          )
        }
    }
  }

  test(
    "On header and body vertices, " +
    "on the edge created successfully but with transaction success, original return should be bubbled up"
  ) {

    PropF.forAllF {
      (
        header: BlockHeader,
        body:   BlockBody
      ) =>
        withMock {
          val graphTxDao = mock[GraphTxDAOMock]
          val currentHeaderVertex = mock[WrappedVertex]
          val bodyVertex = mock[WrappedVertex]
          val headerAndBodyEdge = mock[WrappedEdge]

          (dbFacade
            .getGraph[F](_: Async[F], _: Logger[F]))
            .expects(*, *)
            .returns(graphTxDao)
            .once()

          (vertexFetcher.fetchHeader _)
            .expects(header)
            .returns(currentHeaderVertex.asRight[Failure].pure[F])
            .once()

          (vertexFetcher.fetchBody _)
            .expects(body, header.height)
            .returns(bodyVertex.asRight[Failure].pure[F])
            .once()

          (graphTxDao
            .addEdge(_: WrappedVertex, _: WrappedVertex, _: Option[String]))
            .expects(currentHeaderVertex, bodyVertex, None)
            .returns(headerAndBodyEdge)
            .once()

          (graphTxDao.withEffectfulTransaction[Unit] _)
            .expects(*)
            .onCall((f: F[Either[Failure, Unit]]) => f.ensure(new IllegalArgumentException())(_ == ().asRight[Failure]))
            .once()

          assertIO(
            bodyMediator.mediate(BlockData(header = header, body = body, transactions = null)),
            ().asRight[Failure]
          )
        }
    }
  }

}
