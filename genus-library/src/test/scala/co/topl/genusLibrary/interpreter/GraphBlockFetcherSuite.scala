package co.topl.genusLibrary.interpreter

import cats.effect.IO
import cats.effect.implicits.effectResourceOps
import cats.implicits._
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.models.BlockHeader
import co.topl.genusLibrary.algebras.VertexFetcherAlgebra
import co.topl.genusLibrary.model.{GE, GEs}
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.node.models.BlockBody
import com.tinkerpop.blueprints.Vertex
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class GraphBlockFetcherSuite extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  test("On fetchHeader with throwable response, a FailureMessageWithCause should be returned") {

    PropF.forAllF { (header: BlockHeader) =>
      withMock {

        val res = for {
          graphVertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          expectedTh = new IllegalStateException("boom!")
          _ = (graphVertexFetcher.fetchHeader _)
            .expects(header.id)
            .once()
            .returning(
              (GEs
                .InternalMessageCause("GraphVertexFetcher:fetchHeader", expectedTh): GE)
                .asLeft[Option[Vertex]]
                .pure[F]
            )
          graphBlockFetcher <- GraphBlockFetcher.make[F](graphVertexFetcher)
          _ <- assertIO(
            graphBlockFetcher.fetchHeader(header.id),
            (GEs.InternalMessageCause("GraphVertexFetcher:fetchHeader", expectedTh): GE)
              .asLeft[Option[BlockHeader]]
          ).toResource
        } yield ()

        res.use_
      }
    }
  }

  test("On fetchHeader if an empty iterator is returned, None BlockHeader should be returned") {

    PropF.forAllF { (header: BlockHeader) =>
      withMock {
        val res = for {
          graphVertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          _ = (graphVertexFetcher.fetchHeader _)
            .expects(header.id)
            .returning(Option.empty[Vertex].asRight[GE].pure[F])
          graphBlockFetcher <- GraphBlockFetcher.make[F](graphVertexFetcher)
          _ <- assertIO(
            graphBlockFetcher.fetchHeader(header.id),
            Option.empty[BlockHeader].asRight[GE]
          ).toResource
        } yield ()

        res.use_
      }
    }
  }

  test("On fetchHeaderByHeight with throwable response, a MessageWithCause should be returned") {

    PropF.forAllF { (header: BlockHeader) =>
      withMock {
        val res = for {
          graphVertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          expectedTh = new IllegalStateException("boom!")
          _ = (graphVertexFetcher.fetchHeaderByHeight _)
            .expects(header.height)
            .once()
            .returning(
              (GEs
                .InternalMessageCause("GraphVertexFetcher:fetchHeaderByHeight", expectedTh): GE)
                .asLeft[Option[Vertex]]
                .pure[F]
            )
          graphBlockFetcher <- GraphBlockFetcher.make[F](graphVertexFetcher)
          _ <- assertIO(
            graphBlockFetcher.fetchHeaderByHeight(header.height),
            (GEs.InternalMessageCause("GraphVertexFetcher:fetchHeaderByHeight", expectedTh): GE)
              .asLeft[Option[BlockHeader]]
          ).toResource
        } yield ()

        res.use_
      }
    }
  }

  test("On fetchHeaderByHeight, if an empty iterator is returned, None BlockHeader should be returned") {
    PropF.forAllF { (header: BlockHeader) =>
      withMock {
        val res = for {
          graphVertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          _ = (graphVertexFetcher.fetchHeaderByHeight _)
            .expects(header.height)
            .once()
            .returning(Option.empty[Vertex].asRight[GE].pure[F])
          graphBlockFetcher <- GraphBlockFetcher.make[F](graphVertexFetcher)
          _ <- assertIO(
            graphBlockFetcher.fetchHeaderByHeight(header.height),
            Option.empty[BlockHeader].asRight[GE]
          ).toResource
        } yield ()

        res.use_
      }
    }
  }

  test("On fetchBody, if empty is returned fetching blockHeader Vertex, None BlockBody should be returned") {
    PropF.forAllF { (header: BlockHeader) =>
      withMock {
        val res = for {
          graphVertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          _ = (graphVertexFetcher.fetchHeader _)
            .expects(header.id)
            .once()
            .returning(Option.empty[Vertex].asRight[GE].pure[F])
          graphBlockFetcher <- GraphBlockFetcher.make[F](graphVertexFetcher)
          _ <- assertIO(
            graphBlockFetcher.fetchBody(header.id),
            Option.empty[BlockBody].asRight[GE]
          ).toResource
        } yield ()

        res.use_
      }
    }
  }

  test("On fetchBody, if header Vertex exits, but body vertex does not, None BlockBody should be returned") {
    PropF.forAllF { (header: BlockHeader) =>
      withMock {
        val res = for {
          graphVertexFetcher <- mock[VertexFetcherAlgebra[F]].pure[F].toResource
          headerVertex       <- mock[Vertex].pure[F].toResource

          _ = (graphVertexFetcher.fetchHeader _)
            .expects(header.id)
            .once()
            .returning(headerVertex.some.asRight[GE].pure[F])

          _ = (graphVertexFetcher.fetchBody _)
            .expects(headerVertex)
            .once()
            .returning(Option.empty[Vertex].asRight[GE].pure[F])

          graphBlockFetcher <- GraphBlockFetcher.make[F](graphVertexFetcher)

          _ <- assertIO(
            graphBlockFetcher.fetchBody(header.id),
            Option.empty[BlockBody].asRight[GE]
          ).toResource
        } yield ()

        res.use_
      }
    }
  }

}
