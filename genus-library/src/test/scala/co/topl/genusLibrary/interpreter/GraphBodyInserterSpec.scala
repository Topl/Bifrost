package co.topl.genusLibrary.interpreter

import cats.effect.IO
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.genusLibrary.algebras.mediator.BodyMediatorAlgebra
import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.model.BlockData
import co.topl.genusLibrary.orientDb.GenusGraphMetadata.blockBodySchema
import co.topl.genusLibrary.orientDb.wrapper.WrappedVertex
import co.topl.genusLibrary.orientDb.{GraphTxDAO, StoreFacade, VertexSchema}
import co.topl.node.models.BlockBody
import co.topl.models.generators.node.ModelGenerators.arbitraryNodeBody
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class GraphBodyInserterSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  private trait BodyMediatorMock extends BodyMediatorAlgebra[F]

  private class GraphTxDAOMock extends GraphTxDAO[F](null)

  implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  private val orientDB = mock[StoreFacade]
  private val bodyMediator = mock[BodyMediatorMock]

  val graphBodyInserter = new GraphBodyInserter[F](orientDB, bodyMediator)

  test("On failure to effect transaction, the mediator is not called and the DAO response is returned") {
    PropF.forAllF { blockBody: BlockBody =>
      withMock {
        val blockData = BlockData(null, blockBody, null)
        val graphTxDao = mock[GraphTxDAOMock]
        val wrappedVertex = mock[WrappedVertex]
        val failureLeft = mock[Failure].asLeft[Unit]

        (graphTxDao
          .createVertex(_: BlockBody)(_: VertexSchema[BlockBody]))
          .expects(blockBody, blockBodySchema)
          .returns((blockBody, wrappedVertex))
          .once()

        (orientDB
          .getGraph[F](_: Async[F], _: Logger[F]))
          .expects(*, *)
          .returns(graphTxDao)
          .once()

        (graphTxDao.withEffectfulTransaction[Unit] _)
          .expects(*)
          .returns(failureLeft.pure[F])
          .once()

        val response = graphBodyInserter.insert(blockData)

        assertIO(
          response,
          failureLeft
        )
      }
    }
  }

  test("On success to effect transaction and successful mediator call, the successful mediator response is returned") {
    PropF.forAllF { blockBody: BlockBody =>
      withMock {

        val blockData = BlockData(null, blockBody, null)
        val graphTxDao = mock[GraphTxDAOMock]
        val wrappedVertex = mock[WrappedVertex]
        val mediatorResponse = ().asRight[Failure]

        (graphTxDao
          .createVertex(_: BlockBody)(_: VertexSchema[BlockBody]))
          .expects(blockBody, blockBodySchema)
          .returns((blockBody, wrappedVertex))
          .once()

        (orientDB
          .getGraph[F](_: Async[F], _: Logger[F]))
          .expects(*, *)
          .returns(graphTxDao)
          .once()

        (graphTxDao.withEffectfulTransaction[(BlockBody, WrappedVertex)] _)
          .expects(*)
          .returns((blockBody, wrappedVertex).asRight[Failure].pure[F])
          .once()

        (bodyMediator.mediate _)
          .expects(blockData)
          .returns(mediatorResponse.pure[F])
          .once()

        val response = graphBodyInserter.insert(blockData)

        assertIO(
          response,
          mediatorResponse
        )

      }
    }
  }

  test("On success to effect transaction and failing mediator call, the failing mediator response is returned") {
    PropF.forAllF { blockBody: BlockBody =>
      withMock {

        val blockData = BlockData(null, blockBody, null)
        val graphTxDao = mock[GraphTxDAOMock]
        val wrappedVertex = mock[WrappedVertex]
        val mediatorResponse = mock[Failure].asLeft[Unit]

        (graphTxDao
          .createVertex(_: BlockBody)(_: VertexSchema[BlockBody]))
          .expects(blockBody, blockBodySchema)
          .returns((blockBody, wrappedVertex))
          .once()

        (orientDB
          .getGraph[F](_: Async[F], _: Logger[F]))
          .expects(*, *)
          .returns(graphTxDao)
          .once()

        (graphTxDao.withEffectfulTransaction[(BlockBody, WrappedVertex)] _)
          .expects(*)
          .returns((blockBody, wrappedVertex).asRight[Failure].pure[F])
          .once()

        (bodyMediator.mediate _)
          .expects(blockData)
          .returns(mediatorResponse.pure[F])
          .once()

        val response = graphBodyInserter.insert(blockData)

        assertIO(
          response,
          mediatorResponse
        )

      }
    }
  }

}
