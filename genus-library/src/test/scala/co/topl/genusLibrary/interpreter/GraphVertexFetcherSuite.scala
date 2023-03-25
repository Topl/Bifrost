package co.topl.genusLibrary.interpreter

import cats.data.EitherT
import cats.effect.implicits.effectResourceOps
import cats.effect.{IO, Resource, Sync}
import cats.implicits._
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.models.BlockHeader
import co.topl.genusLibrary.model.{GRE, GREs}
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

  test("On fetchHeader with throwable response, a MessageWithCause should be returned") {

    val expectedTh = new IllegalStateException("boom!")
    val g: OrientGraphNoTx = new OrientGraphNoTx("memory:test") {
      override def getVertices(iKey: String, iValue: Object) = throw expectedTh
    }

    PropF.forAllF { (header: BlockHeader) =>
      withMock {
        val res = for {
          orientGraphNoTx    <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
          graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
          _ <- assertIO(
            graphVertexFetcher.fetchHeader(header.id),
            (GREs.MessageCause("GraphVertexFetcher:fetchHeader", expectedTh): GRE)
              .asLeft[Option[Vertex]]
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
          _ <- assertIO(
            graphVertexFetcher.fetchHeader(header.id),
            Option.empty[Vertex].asRight[GRE]
          ).toResource
        } yield ()

        res.use_
      }
    }
  }

  test("On fetchHeaderByHeight with throwable response, a MessageWithCause should be returned") {

    val expectedTh = new IllegalStateException("boom!")
    val g: OrientGraphNoTx = new OrientGraphNoTx("memory:test") {
      override def getVertices(label: String, iKey: Array[String], iValue: Array[AnyRef]) = throw expectedTh
    }

    PropF.forAllF { (height: Long) =>
      withMock {
        val res = for {
          orientGraphNoTx    <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
          graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
          _ <- assertIO(
            graphVertexFetcher.fetchHeaderByHeight(height),
            (GREs.MessageCause("GraphVertexFetcher:fetchHeaderByHeight", expectedTh): GRE)
              .asLeft[Option[Vertex]]
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

    PropF.forAllF { (height: Long) =>
      withMock {
        val res = for {
          orientGraphNoTx    <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
          graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
          _ <- assertIO(
            graphVertexFetcher.fetchHeaderByHeight(height),
            Option.empty[Vertex].asRight[GRE]
          ).toResource
        } yield ()

        res.use_
      }
    }
  }

  /**
   * This test throws an exception in the console, check out later how to correctly mock an instance of command
   */
  test("On fetchHeaderByDepth, if an empty iterator is returned, a Right None should be returned") {

    val g: OrientGraphNoTx = new OrientGraphNoTx("memory:test") {
      override def getVertices(label: String, iKey: Array[String], iValue: Array[AnyRef]) =
        new java.util.Vector[Vertex]()
    }

    val res = for {
      orientGraphNoTx    <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
      graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
      _                  <- orientGraphNoTx.makeActive().pure[F].toResource
      _ <- assertIO(
        graphVertexFetcher
          .fetchHeaderByDepth(1)
          .map(_.leftMap { case geEx: GREs.MessageCause => geEx.copy(cause = null): GRE }),
        (GREs.MessageCause("GraphVertexFetcher:fetchHeaderByDepth", null): GRE)
          .asLeft[Option[Vertex]]
      ).toResource
    } yield ()

    res.use_

  }

  test("On fetchBody with throwable response, a MessageWithCause should be returned") {

    val expectedTh = new IllegalStateException("boom!")

    val g: OrientGraphNoTx = new OrientGraphNoTx("memory:test") {
      override def getVertices(label: String, iKey: Array[String], iValue: Array[AnyRef]) = throw expectedTh
    }

    withMock {
      val res = for {
        orientGraphNoTx <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
        vertex          <- mock[Vertex].pure[F].toResource
        _ = (() => vertex.getId).expects().once().returning(new Object())
        graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
        _ <- assertIO(
          graphVertexFetcher.fetchBody(vertex),
          (GREs.MessageCause("GraphVertexFetcher:fetchBody", expectedTh): GRE)
            .asLeft[Option[Vertex]]
        ).toResource
      } yield ()

      res.use_
    }

  }

  test("On fetchBody if an empty iterator is returned, a Right None should be returned") {

    val g: OrientGraphNoTx = new OrientGraphNoTx("memory:test") {
      override def getVertices(label: String, iKey: Array[String], iValue: Array[AnyRef]) =
        new java.util.Vector[Vertex]()
    }

    withMock {
      val res = for {
        orientGraphNoTx <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
        vertex          <- mock[Vertex].pure[F].toResource
        _ = (() => vertex.getId).expects().once().returning(new Object())
        graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
        _ <- assertIO(
          graphVertexFetcher.fetchBody(vertex),
          Option.empty[Vertex].asRight[GRE]
        ).toResource
      } yield ()

      res.use_
    }

  }

  test("On fetchTransactions with throwable response, a MessageWithCause should be returned") {

    val expectedTh = new IllegalStateException("boom!")

    val g: OrientGraphNoTx = new OrientGraphNoTx("memory:test") {
      override def getVertices(label: String, iKey: Array[String], iValue: Array[AnyRef]) = throw expectedTh
    }

    withMock {
      val res = for {
        orientGraphNoTx <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
        vertex          <- mock[Vertex].pure[F].toResource
        _ = (() => vertex.getId).expects().once().returning(new Object())
        graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
        _ <- assertIO(
          graphVertexFetcher.fetchTransactions(vertex),
          (GREs.MessageCause("GraphVertexFetcher:fetchTransactions", expectedTh): GRE)
            .asLeft[Iterable[Vertex]]
        ).toResource
      } yield ()

      res.use_
    }

  }

  test("On fetchTransactions if an empty iterator is returned, a Right None should be returned") {

    val g: OrientGraphNoTx = new OrientGraphNoTx("memory:test") {
      override def getVertices(label: String, iKey: Array[String], iValue: Array[AnyRef]) =
        new java.util.Vector[Vertex]()
    }

    withMock {
      val res = for {
        orientGraphNoTx <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
        vertex          <- mock[Vertex].pure[F].toResource
        _ = (() => vertex.getId).expects().once().returning(new Object())
        graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)

        _ <- assertIO(
          EitherT(graphVertexFetcher.fetchTransactions(vertex)).map(_.size).value,
          0.asRight[GRE]
        ).toResource
      } yield ()

      res.use_
    }

  }

}
