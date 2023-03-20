package co.topl.genusLibrary.interpreter

import cats.effect.{IO, Resource, Sync}
import cats.effect.implicits.effectResourceOps
import cats.implicits._
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.models.BlockHeader
import co.topl.genusLibrary.model.{GenusException, GenusExceptions}
import co.topl.models.generators.consensus.ModelGenerators._
import com.tinkerpop.blueprints.impls.orient.OrientGraphNoTx
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scodec.bits.ByteVector

class GraphVertexFetcherSuite extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  test("On fetchHeader no current header vertex, a NoCurrentHeaderVertexFailure should be returned") {

    val g: OrientGraphNoTx = new OrientGraphNoTx("memory:test") {
      override def getVertices(iKey: String, iValue: Object) = throw new IllegalStateException("boom!")
    }

    PropF.forAllF { (header: BlockHeader) =>
      withMock {
        val res = for {
          orientGraphNoTx <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
          graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
          _ <- assertIO(
            graphVertexFetcher.fetchHeader(header.id),
            (GenusExceptions.NoCurrentHeaderVertex(ByteVector(header.id.value.toByteArray)): GenusException)
              .asLeft[Option[BlockHeader]]
          ).toResource
        } yield ()

        res.use_
      }
    }
  }

  test("On fetchHeaderByHeight no current header vertex, a FailureMessage should be returned") {

    val g: OrientGraphNoTx = new OrientGraphNoTx("memory:test") {
      override def getVertices(iKey: String, iValue: Object) = throw new IllegalStateException("boom!")
    }

    PropF.forAllF { (header: BlockHeader) =>
      withMock {
        val res = for {
          orientGraphNoTx <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
          graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
          _ <- assertIO(
            graphVertexFetcher.fetchHeaderByHeight(header.height),
            (GenusExceptions.FailureMessage(
              s"Block header wasn't found for BlockId.height=[${header.height}]"
            ): GenusException)
              .asLeft[Option[BlockHeader]]
          ).toResource
        } yield ()

        res.use_
      }
    }
  }

}
