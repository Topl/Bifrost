package co.topl.genusLibrary.interpreter

import cats.effect.{IO, Resource, Sync}
import cats.effect.implicits.effectResourceOps
import cats.implicits._
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.models.BlockHeader
import co.topl.genusLibrary.model.{GenusException, GenusExceptions}
import co.topl.models.generators.consensus.ModelGenerators._
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.orient.OrientGraphNoTx
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class GraphVertexFetcherSuite extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  test("On fetchHeader with throwable response, a FailureMessageWithCause should be returned") {

    val expectedTh = new IllegalStateException("boom!")
    val g: OrientGraphNoTx = new OrientGraphNoTx("memory:test") {
      override def getVertices(iKey: String, iValue: Object) = throw expectedTh
    }

    PropF.forAllF { (header: BlockHeader) =>
      withMock {
        val res = for {
          orientGraphNoTx    <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
          graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
          graphBlockFetcher  <- GraphBlockFetcher.make[F](orientGraphNoTx, graphVertexFetcher)
          _ <- assertIO(
            graphBlockFetcher.fetchHeader(header.id),
            (GenusExceptions.MessageWithCause("FetchBodyVertex", expectedTh): GenusException)
              .asLeft[Option[BlockHeader]]
          ).toResource
        } yield ()

        res.use_
      }
    }
  }

  test("On fetchHeader if an empty iterator is returned, a Right None should be returned") {
    val g: OrientGraphNoTx = new OrientGraphNoTx("memory:test") {
      override def getVertices(iKey: String, iValue: Object) = new java.util.Vector[Vertex]()
    }

    PropF.forAllF { (header: BlockHeader) =>
      withMock {
        val res = for {
          orientGraphNoTx    <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
          graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
          graphBlockFetcher  <- GraphBlockFetcher.make[F](orientGraphNoTx, graphVertexFetcher)
          _ <- assertIO(
            graphBlockFetcher.fetchHeader(header.id),
            Option.empty[BlockHeader].asRight[GenusException]
          ).toResource
        } yield ()

        res.use_
      }
    }
  }

  test("On fetchHeaderByHeight with throwable response, a FailureMessageWithCause should be returned") {

    val expectedTh = new IllegalStateException("boom!")
    val g: OrientGraphNoTx = new OrientGraphNoTx("memory:test") {
      override def getVertices(label: String, iKey: Array[String], iValue: Array[AnyRef]) = throw expectedTh
    }

    PropF.forAllF { (header: BlockHeader) =>
      withMock {
        val res = for {
          orientGraphNoTx    <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
          graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
          graphBlockFetcher  <- GraphBlockFetcher.make[F](orientGraphNoTx, graphVertexFetcher)
          _ <- assertIO(
            graphBlockFetcher.fetchHeaderByHeight(header.height),
            (GenusExceptions.MessageWithCause("FetchHeaderByHeight", expectedTh): GenusException)
              .asLeft[Option[BlockHeader]]
          ).toResource
        } yield ()

        res.use_
      }
    }
  }

  test("On fetchHeaderByHeight, if an empty iterator is returned, a Right None should be returned") {
    val g: OrientGraphNoTx = new OrientGraphNoTx("memory:test") {
      override def getVertices(label: String, iKey: Array[String], iValue: Array[AnyRef]) =
        new java.util.Vector[Vertex]()
    }

    PropF.forAllF { (header: BlockHeader) =>
      withMock {
        val res = for {
          orientGraphNoTx    <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
          graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
          graphBlockFetcher  <- GraphBlockFetcher.make[F](orientGraphNoTx, graphVertexFetcher)
          _ <- assertIO(
            graphBlockFetcher.fetchHeaderByHeight(header.height),
            Option.empty[BlockHeader].asRight[GenusException]
          ).toResource
        } yield ()

        res.use_
      }
    }
  }

}
