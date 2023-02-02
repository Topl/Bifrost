package co.topl.genusLibrary.orientDb

import cats.effect.kernel.Async
import cats.effect.IO
import cats.implicits._
import co.topl.genusLibrary.failure.{Failure, Failures}
import co.topl.genusLibrary.orientDb.GenusGraphMetadata.{blockBodySchema, blockHeaderSchema}
import co.topl.genusLibrary.orientDb.wrapper.WrappedVertex
import co.topl.genusLibrary.utils.BlockUtils
import co.topl.models.ModelGenerators._
import co.topl.models.{BlockBody, BlockHeader}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scodec.bits.ByteVector

class GraphVertexFetcherSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  private val dbFacade = mock[StoreFacade]
  private val blockUtils = mock[BlockUtils]

  val graphVertexFetcher = new GraphVertexFetcher[F](dbFacade, blockUtils)

  test("On no current header vertex, a NoCurrentHeaderVertexFailure should be returned") {
    PropF.forAllF {
      (
        header:  BlockHeader,
        blockId: Array[Byte]
      ) =>
        withMock {
          (blockUtils.getBlockId _)
            .expects(header)
            .returns(blockId)
            .once()

          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockHeaderSchema.name, "blockId", blockId, *)
            .returns(Option.empty[WrappedVertex].pure[F])
            .once()

          assertIO(
            graphVertexFetcher.fetchHeader(header),
            Failures.NoCurrentHeaderVertexFailure(ByteVector(blockId)).asLeft[WrappedVertex]
          )
        }
    }
  }

  test("On current header vertex, said vertex should be returned") {
    PropF.forAllF {
      (
        header:  BlockHeader,
        blockId: Array[Byte]
      ) =>
        withMock {

          val currentHeaderVertex = mock[WrappedVertex]

          (blockUtils.getBlockId _)
            .expects(header)
            .returns(blockId)
            .once()

          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockHeaderSchema.name, "blockId", blockId, *)
            .returns(currentHeaderVertex.some.pure[F])
            .once()

          assertIO(
            graphVertexFetcher.fetchHeader(header),
            currentHeaderVertex.asRight[Failure]
          )
        }
    }
  }

  test("On no previous header vertex, a NoPreviousHeaderVertexFailure should be returned") {
    PropF.forAllF {
      (
        header:        BlockHeader,
        parentBlockId: Array[Byte]
      ) =>
        withMock {
          (blockUtils.getParentBlockId _)
            .expects(header)
            .returns(parentBlockId)
            .once()

          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockHeaderSchema.name, "blockId", parentBlockId, *)
            .returns(Option.empty[WrappedVertex].pure[F])
            .once()

          assertIO(
            graphVertexFetcher.fetchPreviousHeader(header),
            Failures.NoPreviousHeaderVertexFailure(ByteVector(parentBlockId)).asLeft[WrappedVertex]
          )
        }
    }
  }

  test("On previous header vertex, said vertex should be returned") {
    PropF.forAllF {
      (
        header:        BlockHeader,
        parentBlockId: Array[Byte]
      ) =>
        withMock {

          val previousHeaderVertex = mock[WrappedVertex]

          (blockUtils.getParentBlockId _)
            .expects(header)
            .returns(parentBlockId)
            .once()

          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockHeaderSchema.name, "blockId", parentBlockId, *)
            .returns(previousHeaderVertex.some.pure[F])
            .once()

          assertIO(
            graphVertexFetcher.fetchPreviousHeader(header),
            previousHeaderVertex.asRight[Failure]
          )
        }
    }
  }

  test("On no body vertex, a NoCurrentBodyVertexFailure should be returned") {
    PropF.forAllF {
      (
        body:        BlockBody,
        bodyArray:   Array[Byte],
        blockHeight: Long
      ) =>
        withMock {

          (blockUtils.blockBodyToByteArray _)
            .expects(body)
            .returns(bodyArray)
            .once()

          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockBodySchema.name, "transactionIds", bodyArray, *)
            .returns(Option.empty[WrappedVertex].pure[F])
            .once()

          assertIO(
            graphVertexFetcher.fetchBody(body, blockHeight),
            Failures.NoCurrentBodyVertexFailure(ByteVector(bodyArray)).asLeft[WrappedVertex]
          )
        }
    }
  }

  test("On body vertex, said body vertex should be returned") {
    PropF.forAllF {
      (
        body:        BlockBody,
        bodyArray:   Array[Byte],
        blockHeight: Long
      ) =>
        withMock {

          val bodyVertex = mock[WrappedVertex]

          (blockUtils.blockBodyToByteArray _)
            .expects(body)
            .returns(bodyArray)
            .once()

          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockBodySchema.name, "transactionIds", bodyArray, *)
            .returns(bodyVertex.some.pure[F])
            .once()

          assertIO(
            graphVertexFetcher.fetchBody(body, blockHeight),
            bodyVertex.asRight[Failure]
          )
        }
    }
  }

}
