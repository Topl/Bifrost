package co.topl.genusLibrary.interpreter

import cats.effect.IO
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.genusLibrary.Txo
import co.topl.genusLibrary.failure.{Failure, Failures}
import co.topl.genusLibrary.model.BlockData
import co.topl.genusLibrary.orientDb.GenusGraphMetadata.{blockBodySchema, blockHeaderSchema}
import co.topl.genusLibrary.orientDb.wrapper.{WrappedEdge, WrappedVertex}
import co.topl.genusLibrary.orientDb.{GraphTxDAO, StoreFacade}
import co.topl.genusLibrary.utils.BlockUtils
import co.topl.models.ModelGenerators._
import co.topl.{models => legacyModels}
import legacyModels.Transaction
import co.topl.consensus.models.BlockHeader
import co.topl.node.models.BlockBody
import legacyModels.generators.node.ModelGenerators.arbitraryNodeBody
import co.topl.proto.models.TypedEvidence
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scodec.bits.ByteVector

class GraphMediatorSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  private class GraphTxDAOMock extends GraphTxDAO[F](null)

  implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  private val dbFacade = mock[StoreFacade]
  private val blockUtils = mock[BlockUtils]

  private val graphMediator = new GraphMediator[F](dbFacade, blockUtils) {
    override def afterBodyInserted(body: BlockBody, block: BlockData): F[Either[Failure, Unit]] = ???
    override def afterTxInserted(transaction: Transaction, block: BlockData): F[Either[Failure, Unit]] = ???
    override def afterAddressInserted(address: TypedEvidence, block: BlockData): F[Either[Failure, Unit]] = ???
    override def afterTxoInserted(txo: Txo, block: BlockData): F[Either[Failure, Unit]] = ???
    override def afterAddressStateInserted(block: BlockData): F[Either[Failure, Unit]] = ???
  }

  test("On no current header vertex, a NoCurrentHeaderVertexFailure should be returned") {

    PropF.forAllF {
      (
        header:  BlockHeader,
        blockId: Array[Byte]
      ) =>
        withMock {
          val graphTxDao = mock[GraphTxDAOMock]

          val leftFailure = Failures.NoCurrentHeaderVertexFailure(ByteVector(blockId)).asLeft[Unit]

          (dbFacade
            .getGraph[F](_: Async[F], _: Logger[F]))
            .expects(*, *)
            .returns(graphTxDao)
            .once()

          (blockUtils.getBlockId _)
            .expects(header)
            .returns(blockId)
            .once()

          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockHeaderSchema.name, "blockId", blockId, *)
            .returns(Option.empty[WrappedVertex].pure[F])
            .once()

          (graphTxDao.withEffectfulTransaction[Unit] _)
            .expects(*)
            .onCall((f: F[Either[Failure, Unit]]) => f.ensure(new IllegalArgumentException())(_ == leftFailure))
            .once()

          assertIO(
            graphMediator.afterHeaderInserted(BlockData(header = header, body = null, transactions = null)),
            leftFailure
          )
        }
    }

  }

  test("On no previous header vertex, a NoCurrentHeaderVertexFailure should be returned") {

    PropF.forAllF {
      (
        header:        BlockHeader,
        blockId:       Array[Byte],
        parentBlockId: Array[Byte]
      ) =>
        withMock {
          val graphTxDao = mock[GraphTxDAOMock]
          val currentHeaderVertex = mock[WrappedVertex]

          val leftFailure = Failures.NoPreviousHeaderVertexFailure(ByteVector(parentBlockId)).asLeft[Unit]

          (dbFacade
            .getGraph[F](_: Async[F], _: Logger[F]))
            .expects(*, *)
            .returns(graphTxDao)
            .once()

          (blockUtils.getBlockId _)
            .expects(header)
            .returns(blockId)
            .once()

          (blockUtils.getParentBlockId _)
            .expects(header)
            .returns(parentBlockId)
            .once()

          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockHeaderSchema.name, "blockId", blockId, *)
            .returns(currentHeaderVertex.some.pure[F])
            .once()

          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockHeaderSchema.name, "blockId", parentBlockId, *)
            .returns(Option.empty[WrappedVertex].pure[F])
            .once()

          (graphTxDao.withEffectfulTransaction[Unit] _)
            .expects(*)
            .onCall((f: F[Either[Failure, Unit]]) => f.ensure(new IllegalArgumentException())(_ == leftFailure))
            .once()

          assertIO(
            graphMediator.afterHeaderInserted(BlockData(header = header, body = null, transactions = null)),
            leftFailure
          )
        }
    }

  }

  test("On no body vertex, a NoCurrentBodyVertexFailure should be returned") {

    PropF.forAllF {
      (
        header:        BlockHeader,
        body:          BlockBody,
        blockId:       Array[Byte],
        parentBlockId: Array[Byte],
        bodyArray:     Array[Byte]
      ) =>
        withMock {
          val graphTxDao = mock[GraphTxDAOMock]
          val currentHeaderVertex = mock[WrappedVertex]
          val previousHeaderVertex = mock[WrappedVertex]

          val leftFailure = Failures.NoCurrentBodyVertexFailure(ByteVector(bodyArray)).asLeft[Unit]

          (dbFacade
            .getGraph[F](_: Async[F], _: Logger[F]))
            .expects(*, *)
            .returns(graphTxDao)
            .once()

          (blockUtils.getBlockId _)
            .expects(header)
            .returns(blockId)
            .once()

          (blockUtils.getParentBlockId _)
            .expects(header)
            .returns(parentBlockId)
            .once()

          (blockUtils.blockBodyToByteArray _)
            .expects(body)
            .returns(bodyArray)
            .once()

          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockHeaderSchema.name, "blockId", blockId, *)
            .returns(currentHeaderVertex.some.pure[F])
            .once()

          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockHeaderSchema.name, "blockId", parentBlockId, *)
            .returns(previousHeaderVertex.some.pure[F])
            .once()

          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockBodySchema.name, "transactionIds", bodyArray, *)
            .returns(Option.empty[WrappedVertex].pure[F])
            .once()

          (graphTxDao.withEffectfulTransaction[Unit] _)
            .expects(*)
            .onCall((f: F[Either[Failure, Unit]]) => f.ensure(new IllegalArgumentException())(_ == leftFailure))
            .once()

          assertIO(
            graphMediator.afterHeaderInserted(BlockData(header = header, body = body, transactions = null)),
            leftFailure
          )
        }
    }
  }

  test(
    "On header, previous header and body vertices, " +
    "on all edges created successfully but with transaction failure, it should be bubbled up"
  ) {

    PropF.forAllF {
      (
        header:        BlockHeader,
        body:          BlockBody,
        blockId:       Array[Byte],
        parentBlockId: Array[Byte],
        bodyArray:     Array[Byte]
      ) =>
        withMock {
          val graphTxDao = mock[GraphTxDAOMock]
          val currentHeaderVertex = mock[WrappedVertex]
          val previousHeaderVertex = mock[WrappedVertex]
          val bodyVertex = mock[WrappedVertex]
          val headersEdge = mock[WrappedEdge]
          val headerBodyEdge = mock[WrappedEdge]

          val leftFailure = mock[Failure].asLeft[Unit]

          (dbFacade
            .getGraph[F](_: Async[F], _: Logger[F]))
            .expects(*, *)
            .returns(graphTxDao)
            .once()

          (blockUtils.getBlockId _)
            .expects(header)
            .returns(blockId)
            .once()

          (blockUtils.getParentBlockId _)
            .expects(header)
            .returns(parentBlockId)
            .once()

          (blockUtils.blockBodyToByteArray _)
            .expects(body)
            .returns(bodyArray)
            .once()

          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockHeaderSchema.name, "blockId", blockId, *)
            .returns(currentHeaderVertex.some.pure[F])
            .once()

          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockHeaderSchema.name, "blockId", parentBlockId, *)
            .returns(previousHeaderVertex.some.pure[F])
            .once()

          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockBodySchema.name, "transactionIds", bodyArray, *)
            .returns(bodyVertex.some.pure[F])
            .once()

          (graphTxDao
            .addEdge(_: WrappedVertex, _: WrappedVertex, _: Option[String]))
            .expects(previousHeaderVertex, currentHeaderVertex, None)
            .returns(headersEdge)
            .once()

          (graphTxDao
            .addEdge(_: WrappedVertex, _: WrappedVertex, _: Option[String]))
            .expects(currentHeaderVertex, bodyVertex, None)
            .returns(headerBodyEdge)
            .once()

          (graphTxDao.withEffectfulTransaction[Unit] _)
            .expects(*)
            .onCall((f: F[Either[Failure, Unit]]) =>
              f.ensure(new IllegalArgumentException())(_ == ().asRight[Failure])
                .map(_ => leftFailure)
            )
            .once()

          assertIO(
            graphMediator.afterHeaderInserted(BlockData(header = header, body = body, transactions = null)),
            leftFailure
          )
        }
    }
  }

  test(
    "On header, previous header and body vertices, " +
    "on all edges created successfully but with transaction success, original return should be bubbled up"
  ) {

    PropF.forAllF {
      (
        header:        BlockHeader,
        body:          BlockBody,
        blockId:       Array[Byte],
        parentBlockId: Array[Byte],
        bodyArray:     Array[Byte]
      ) =>
        withMock {
          val graphTxDao = mock[GraphTxDAOMock]
          val currentHeaderVertex = mock[WrappedVertex]
          val previousHeaderVertex = mock[WrappedVertex]
          val bodyVertex = mock[WrappedVertex]
          val headersEdge = mock[WrappedEdge]
          val headerBodyEdge = mock[WrappedEdge]

          (dbFacade
            .getGraph[F](_: Async[F], _: Logger[F]))
            .expects(*, *)
            .returns(graphTxDao)
            .once()

          (blockUtils.getBlockId _)
            .expects(header)
            .returns(blockId)
            .once()

          (blockUtils.getParentBlockId _)
            .expects(header)
            .returns(parentBlockId)
            .once()

          (blockUtils.blockBodyToByteArray _)
            .expects(body)
            .returns(bodyArray)
            .once()

          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockHeaderSchema.name, "blockId", blockId, *)
            .returns(currentHeaderVertex.some.pure[F])
            .once()

          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockHeaderSchema.name, "blockId", parentBlockId, *)
            .returns(previousHeaderVertex.some.pure[F])
            .once()

          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockBodySchema.name, "transactionIds", bodyArray, *)
            .returns(bodyVertex.some.pure[F])
            .once()

          (graphTxDao
            .addEdge(_: WrappedVertex, _: WrappedVertex, _: Option[String]))
            .expects(previousHeaderVertex, currentHeaderVertex, None)
            .returns(headersEdge)
            .once()

          (graphTxDao
            .addEdge(_: WrappedVertex, _: WrappedVertex, _: Option[String]))
            .expects(currentHeaderVertex, bodyVertex, None)
            .returns(headerBodyEdge)
            .once()

          (graphTxDao.withEffectfulTransaction[Unit] _)
            .expects(*)
            .onCall((f: F[Either[Failure, Unit]]) => f.ensure(new IllegalArgumentException())(_ == ().asRight[Failure]))
            .once()

          assertIO(
            graphMediator.afterHeaderInserted(BlockData(header = header, body = body, transactions = null)),
            ().asRight[Failure]
          )
        }
    }
  }

}
