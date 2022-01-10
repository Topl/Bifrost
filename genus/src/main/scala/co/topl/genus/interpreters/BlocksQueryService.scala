package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.genus.algebras.DatabaseClientAlg
import co.topl.genus.filters.BlockFilter
import co.topl.genus.services.blocks_query.{BlocksQuery, QueryBlocksReq, QueryBlocksRes}
import co.topl.genus.types.Block

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

object BlocksQueryService {

  object Eval {

    def make(
      databaseClient: DatabaseClientAlg[IO, Source[*, NotUsed]],
      timeout:        FiniteDuration
    )(implicit
      executionContext: ExecutionContext,
      materializer:     Materializer
    ): BlocksQuery =
      (in: QueryBlocksReq) =>
        (for {
          blocksSource <-
            databaseClient.queryBlocks(
              in.filter.getOrElse(BlockFilter.of(BlockFilter.FilterType.All(BlockFilter.AllFilter())))
            )
          blocksResult <- IO.fromFuture(blocksSource.take(1000).runWith(Sink.seq).pure[IO])
        } yield QueryBlocksRes(blocksResult))
          .timeout(timeout)
          .unsafeToFuture()
  }

  object Mock {

    def make: BlocksQuery =
      (_: QueryBlocksReq) =>
        Future.successful(
          QueryBlocksRes(
            List(
              Block(
                id = "test-block-id-1",
                height = 44
              ),
              Block(
                id = "test-block-id-2",
                height = 99
              )
            )
          )
        )
  }
}
