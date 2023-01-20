package co.topl.genusLibrary.interpreter

import cats.effect.IO
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.genusLibrary.algebras.Mediator
import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.model.BlockData
import co.topl.genusLibrary.orientDb.GenusGraphMetadata.blockHeaderSchema
import co.topl.genusLibrary.orientDb.wrapper.WrappedVertex
import co.topl.genusLibrary.orientDb.{DBFacade, GraphTxDAO, VertexSchema}
import co.topl.models.BlockHeader
import co.topl.models.ModelGenerators._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class GraphHeaderInserterSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  private trait MediatorMock extends Mediator[F]

  private class GraphTxDAOMock extends GraphTxDAO[F](null)

  implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  private val orientDB = mock[DBFacade]
  private val mediator = mock[MediatorMock]

  val graphHeaderInserter = new GraphHeaderInserter[F](orientDB, mediator)

  test("On failure to effect transaction, the mediator is not called and the DAO response is returned") {
    PropF.forAllF { blockHeader: BlockHeader =>
      withMock {
        val blockData = BlockData(blockHeader, null, null)
        val graphTxDao = mock[GraphTxDAOMock]
        val wrappedVertex = mock[WrappedVertex]
        val failureLeft = mock[Failure].asLeft[Unit]

        (graphTxDao
          .createVertex(_: BlockHeader)(_: VertexSchema[BlockHeader]))
          .expects(blockHeader, blockHeaderSchema)
          .returns((blockHeader, wrappedVertex))
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

        val response = graphHeaderInserter.insert(blockData)

        assertIO(
          response,
          failureLeft
        )
      }
    }
  }

  test("On success to effect transaction and successful mediator call, the successful mediator response is returned") {
    PropF.forAllF { blockHeader: BlockHeader =>
      withMock {

        val blockData = BlockData(blockHeader, null, null)
        val graphTxDao = mock[GraphTxDAOMock]
        val wrappedVertex = mock[WrappedVertex]
        val mediatorResponse = ().asRight[Failure]

        (graphTxDao
          .createVertex(_: BlockHeader)(_: VertexSchema[BlockHeader]))
          .expects(blockHeader, blockHeaderSchema)
          .returns((blockHeader, wrappedVertex))
          .once()

        (orientDB
          .getGraph[F](_: Async[F], _: Logger[F]))
          .expects(*, *)
          .returns(graphTxDao)
          .once()

        (graphTxDao.withEffectfulTransaction[(BlockHeader, WrappedVertex)] _)
          .expects(*)
          .returns((blockHeader, wrappedVertex).asRight[Failure].pure[F])
          .once()

        (mediator.afterHeaderInserted _)
          .expects(blockData)
          .returns(mediatorResponse.pure[F])
          .once

        val response = graphHeaderInserter.insert(blockData)

        assertIO(
          response,
          mediatorResponse
        )

      }
    }
  }

  test("On success to effect transaction and failing mediator call, the failing mediator response is returned") {
    PropF.forAllF { blockHeader: BlockHeader =>
      withMock {

        val blockData = BlockData(blockHeader, null, null)
        val graphTxDao = mock[GraphTxDAOMock]
        val wrappedVertex = mock[WrappedVertex]
        val mediatorResponse = mock[Failure].asLeft[Unit]

        (graphTxDao
          .createVertex(_: BlockHeader)(_: VertexSchema[BlockHeader]))
          .expects(blockHeader, blockHeaderSchema)
          .returns((blockHeader, wrappedVertex))
          .once()

        (orientDB
          .getGraph[F](_: Async[F], _: Logger[F]))
          .expects(*, *)
          .returns(graphTxDao)
          .once()

        (graphTxDao.withEffectfulTransaction[(BlockHeader, WrappedVertex)] _)
          .expects(*)
          .returns((blockHeader, wrappedVertex).asRight[Failure].pure[F])
          .once()

        (mediator.afterHeaderInserted _)
          .expects(blockData)
          .returns(mediatorResponse.pure[F])
          .once

        val response = graphHeaderInserter.insert(blockData)

        assertIO(
          response,
          mediatorResponse
        )

      }
    }
  }

}
