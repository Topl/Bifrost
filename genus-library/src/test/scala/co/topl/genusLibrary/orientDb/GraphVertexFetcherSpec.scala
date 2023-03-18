package co.topl.genusLibrary.orientDb

import cats.effect.kernel.Async
import cats.effect.IO
import cats.implicits._
import co.topl.consensus.models.BlockHeader
import co.topl.genusLibrary.failure.{Failure, Failures}
import co.topl.genusLibrary.orientDb.schema.VertexSchemaInstances.instances.{blockBodySchema, blockHeaderSchema}
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.genusLibrary.orientDb.wrapper.WrappedVertex
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.models.generators.node.ModelGenerators.arbitraryNodeBody
import co.topl.node.models.BlockBody
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.matchers.ArgCapture.CaptureOne
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scodec.bits.ByteVector

class GraphVertexFetcherSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  private val dbFacade = mock[StoreFacade]

  val graphVertexFetcher = new GraphVertexFetcher[F](dbFacade)

  test("On no current header vertex, a NoCurrentHeaderVertexFailure should be returned") {
    PropF.forAllF {
      (
        header: BlockHeader
      ) =>
        withMock {
          val blockIdValue = CaptureOne[Array[Byte]]()
          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockHeaderSchema.name, "blockId", capture(blockIdValue), *)
            .returns(Option.empty[WrappedVertex].pure[F])
            .once()

          assertIO(
            graphVertexFetcher.fetchHeader(header),
            Failures.NoCurrentHeaderVertexFailure(ByteVector(header.id.value.toByteArray)).asLeft[WrappedVertex]
          )
          assertIOBoolean(blockIdValue.value.sameElements(header.id.value.toByteArray).pure[F])
        }
    }
  }

  test("On current header vertex, said vertex should be returned") {
    PropF.forAllF {
      (
        header: BlockHeader
      ) =>
        withMock {
          val currentHeaderVertex = mock[WrappedVertex]
          val blockIdValue = CaptureOne[Array[Byte]]()
          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockHeaderSchema.name, "blockId", capture(blockIdValue), *)
            .returns(currentHeaderVertex.some.pure[F])
            .once()

          assertIO(
            graphVertexFetcher.fetchHeader(header),
            currentHeaderVertex.asRight[Failure]
          )
          assertIOBoolean(blockIdValue.value.sameElements(header.id.value.toByteArray).pure[F])
        }
    }
  }

  test("On no previous header vertex, a NoPreviousHeaderVertexFailure should be returned") {
    PropF.forAllF {
      (
        header: BlockHeader
      ) =>
        withMock {
          val blockIdValue = CaptureOne[Array[Byte]]()
          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockHeaderSchema.name, "blockId", capture(blockIdValue), *)
            .returns(Option.empty[WrappedVertex].pure[F])
            .once()

          val parentHeaderId = header.parentHeaderId.value.toByteArray
          assertIO(
            graphVertexFetcher.fetchPreviousHeader(header),
            Failures.NoPreviousHeaderVertexFailure(ByteVector(parentHeaderId)).asLeft[WrappedVertex]
          )
          assertIOBoolean(blockIdValue.value.sameElements(parentHeaderId).pure[F])
        }
    }
  }

  test("On previous header vertex, said vertex should be returned") {
    PropF.forAllF {
      (
        header: BlockHeader
      ) =>
        withMock {
          val previousHeaderVertex = mock[WrappedVertex]
          val blockIdValue = CaptureOne[Array[Byte]]()
          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockHeaderSchema.name, "blockId", capture(blockIdValue), *)
            .returns(previousHeaderVertex.some.pure[F])
            .once()

          val parentHeaderId = header.parentHeaderId.value.toByteArray
          assertIO(
            graphVertexFetcher.fetchPreviousHeader(header),
            previousHeaderVertex.asRight[Failure]
          )
          assertIOBoolean(blockIdValue.value.sameElements(parentHeaderId).pure[F])
        }
    }
  }

  test("On no body vertex, a NoCurrentBodyVertexFailure should be returned") {
    PropF.forAllF {
      (
        body:        BlockBody,
        blockHeight: Long
      ) =>
        withMock {
          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockBodySchema.name, "transactionIds", *, *)
            .returns(Option.empty[WrappedVertex].pure[F])
            .once()

          assertIO(
            graphVertexFetcher.fetchBody(body, blockHeight),
            Failures.NoCurrentBodyVertexFailure(ByteVector(body.toByteArray)).asLeft[WrappedVertex]
          )
        }
    }
  }

  test("On body vertex, said body vertex should be returned") {
    PropF.forAllF {
      (
        body:        BlockBody,
        blockHeight: Long
      ) =>
        withMock {

          val bodyVertex = mock[WrappedVertex]

          (dbFacade
            .getVertexByField[F](_: String, _: String, _: AnyRef)(_: Async[F]))
            .expects(blockBodySchema.name, "transactionIds", *, *)
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
