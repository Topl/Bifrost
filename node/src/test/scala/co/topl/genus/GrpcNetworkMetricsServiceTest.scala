package co.topl.genus

import cats.effect.IO
import cats.implicits._
import co.topl.genus.services.TxoStats
import co.topl.genus.services._
import co.topl.genusLibrary.algebras.VertexFetcherAlgebra
import co.topl.genusLibrary.model.{GE, GEs}
import io.grpc.{Metadata, StatusException}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory

class GrpcNetworkMetricsServiceTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  test("getTxoStats: Exceptions") {

    withMock {
      val vertexFetcher = mock[VertexFetcherAlgebra[F]]
      val underTest = new GrpcNetworkMetricsService[F](vertexFetcher)

      (() => vertexFetcher.fetchTxoStats())
        .expects()
        .once()
        .returning((GEs.Internal(new IllegalStateException("Boom!")): GE).asLeft[TxoStats].pure[F])

      for {
        _ <- interceptMessageIO[StatusException]("INTERNAL: Boom!")(
          underTest.getTxoStats(GetTxoStatsReq(), new Metadata())
        )
      } yield ()
    }

  }

  test("getTxoStats: Default Stats") {

    withMock {
      val vertexFetcher = mock[VertexFetcherAlgebra[F]]
      val underTest = new GrpcNetworkMetricsService[F](vertexFetcher)

      (() => vertexFetcher.fetchTxoStats())
        .expects()
        .once()
        .returning(TxoStats.defaultInstance.asRight[GE].pure[F])

      for {
        res <- underTest.getTxoStats(GetTxoStatsReq(), new Metadata())
        _ = assert(res.txos.total == 0)
      } yield ()
    }

  }

}
