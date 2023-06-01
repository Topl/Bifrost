package co.topl.genusLibrary.interpreter

import cats.data.EitherT
import cats.effect.implicits.effectResourceOps
import cats.effect.{Resource, Sync}
import cats.implicits._
import co.topl.brambl.models.{LockAddress, TransactionOutputAddress}
import co.topl.brambl.generators.ModelGenerators._
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.models.BlockHeader
import co.topl.genusLibrary.DbFixtureUtil
import co.topl.genusLibrary.model.{GE, GEs}
import co.topl.genusLibrary.orientDb.OrientThread
import co.topl.models.generators.consensus.ModelGenerators._
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.orient.OrientGraphNoTx
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class GraphVertexFetcherTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with DbFixtureUtil {

  test("On fetchHeader with throwable response, a MessageWithCause should be returned") {

    val expectedTh = new IllegalStateException("boom!")
    val g: OrientGraphNoTx = new OrientGraphNoTx("memory:test") {
      override def getVertices(iKey: String, iValue: Object) = throw expectedTh
    }

    PropF.forAllF { (header: BlockHeader) =>
      withMock {
        val res = for {
          implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
          orientGraphNoTx    <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
          graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
          _ <- assertIO(
            graphVertexFetcher.fetchHeader(header.id),
            (GEs.InternalMessageCause("GraphVertexFetcher:fetchHeader", expectedTh): GE)
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
          implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
          orientGraphNoTx    <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
          graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
          _ <- assertIO(
            graphVertexFetcher.fetchHeader(header.id),
            Option.empty[Vertex].asRight[GE]
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
          implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
          orientGraphNoTx    <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
          graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
          _ <- assertIO(
            graphVertexFetcher.fetchHeaderByHeight(height),
            (GEs.InternalMessageCause("GraphVertexFetcher:fetchHeaderByHeight", expectedTh): GE)
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
          implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
          orientGraphNoTx    <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
          graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
          _ <- assertIO(graphVertexFetcher.fetchHeaderByHeight(height), Option.empty[Vertex].asRight[GE]).toResource
        } yield ()

        res.use_
      }
    }
  }

  test("On fetchHeaderByDepth, if an empty iterator is returned, a Right None should be returned") {

    val g: OrientGraphNoTx = new OrientGraphNoTx("memory:test") {
      override def getVertices(label: String, iKey: Array[String], iValue: Array[AnyRef]) =
        new java.util.Vector[Vertex]()
    }

    val res = for {
      implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
      orientGraphNoTx <- Resource.make(Sync[F].blocking(g))(g =>
        Sync[F].delay {
          g.drop()
          g.shutdown()
        }
      )
      graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
      _                  <- Sync[F].blocking(orientGraphNoTx.createVertexType("BlockHeader")).toResource

      _ <- assertIO(graphVertexFetcher.fetchHeaderByDepth(1), Option.empty[Vertex].asRight[GE]).toResource
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
        implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
        orientGraphNoTx                          <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
        vertex                                   <- mock[Vertex].pure[F].toResource
        _ = (() => vertex.getId).expects().once().returning(new Object())
        graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
        _ <- assertIO(
          graphVertexFetcher.fetchBody(vertex),
          (GEs.InternalMessageCause("GraphVertexFetcher:fetchBody", expectedTh): GE)
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
        implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
        orientGraphNoTx                          <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
        vertex                                   <- mock[Vertex].pure[F].toResource
        _ = (() => vertex.getId).expects().once().returning(new Object())
        graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
        _ <- assertIO(graphVertexFetcher.fetchBody(vertex), Option.empty[Vertex].asRight[GE]).toResource
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
        implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
        orientGraphNoTx                          <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
        vertex                                   <- mock[Vertex].pure[F].toResource
        _ = (() => vertex.getId).expects().once().returning(new Object())
        graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
        _ <- assertIO(
          graphVertexFetcher.fetchTransactions(vertex),
          (GEs.InternalMessageCause("GraphVertexFetcher:fetchTransactions", expectedTh): GE)
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
        implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
        orientGraphNoTx                          <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
        vertex                                   <- mock[Vertex].pure[F].toResource
        _ = (() => vertex.getId).expects().once().returning(new Object())
        graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)

        _ <- assertIO(EitherT(graphVertexFetcher.fetchTransactions(vertex)).map(_.size).value, 0.asRight[GE]).toResource
      } yield ()

      res.use_
    }

  }

  test("On fetchLockAddress with throwable response, a MessageWithCause should be returned") {

    val expectedTh = new IllegalStateException("boom!")
    val g: OrientGraphNoTx = new OrientGraphNoTx("memory:test") {
      override def getVertices(iKey: String, iValue: Object) = throw expectedTh
    }

    PropF.forAllF { (lockAddress: LockAddress) =>
      withMock {
        val res = for {
          implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
          orientGraphNoTx    <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
          graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
          _ <- assertIO(
            graphVertexFetcher.fetchLockAddress(lockAddress),
            (GEs.InternalMessageCause("GraphVertexFetcher:fetchLockAddress", expectedTh): GE)
              .asLeft[Option[Vertex]]
          ).toResource
        } yield ()

        res.use_
      }
    }
  }

  test("On fetchLockAddress if an empty iterator is returned, a Right None should be returned") {

    val g: OrientGraphNoTx = new OrientGraphNoTx("memory:test") {
      override def getVertices(iKey: String, iValue: Object) = new java.util.Vector[Vertex]()
    }

    PropF.forAllF { (lockAddress: LockAddress) =>
      withMock {
        val res = for {
          implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
          orientGraphNoTx    <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
          graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)

          _ <- assertIO(
            graphVertexFetcher.fetchLockAddress(lockAddress),
            Option.empty[Vertex].asRight[GE]
          ).toResource
        } yield ()

        res.use_
      }

    }
  }

  test("On fetchTxo with throwable response, a MessageWithCause should be returned") {

    val expectedTh = new IllegalStateException("boom!")
    val g: OrientGraphNoTx = new OrientGraphNoTx("memory:test") {
      override def getVertices(iKey: String, iValue: Object) = throw expectedTh
    }

    PropF.forAllF { (transactionOutputAddress: TransactionOutputAddress) =>
      withMock {
        val res = for {
          implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
          orientGraphNoTx    <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
          graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)
          _ <- assertIO(
            graphVertexFetcher.fetchTxo(transactionOutputAddress),
            (GEs.InternalMessageCause("GraphVertexFetcher:fetchTxo", expectedTh): GE)
              .asLeft[Option[Vertex]]
          ).toResource
        } yield ()

        res.use_
      }
    }
  }

  test("On fetchTxo if an empty iterator is returned, a Right None should be returned") {

    val g: OrientGraphNoTx = new OrientGraphNoTx("memory:test") {
      override def getVertices(iKey: String, iValue: Object) = new java.util.Vector[Vertex]()
    }

    PropF.forAllF { (transactionOutputAddress: TransactionOutputAddress) =>
      withMock {
        val res = for {
          implicit0(orientThread: OrientThread[F]) <- OrientThread.create[F]
          orientGraphNoTx    <- Resource.make(Sync[F].blocking(g))(g => Sync[F].delay(g.shutdown()))
          graphVertexFetcher <- GraphVertexFetcher.make[F](orientGraphNoTx)

          _ <- assertIO(
            graphVertexFetcher.fetchTxo(transactionOutputAddress),
            Option.empty[Vertex].asRight[GE]
          ).toResource
        } yield ()

        res.use_
      }

    }
  }

}
