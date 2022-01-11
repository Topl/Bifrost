package co.topl.genus.interpreters.queryservices

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import cats.MonadThrow
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.genus.algebras.DatabaseClientAlg
import co.topl.genus.filters.BlockFilter
import co.topl.genus.interpreters.queryservices.implicits._
import co.topl.genus.services.blocks_query.{BlocksQuery, QueryBlocksReq, QueryBlocksRes}
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.Block

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

object BlocksQueryService {

  object Eval {

    def make[F[_]: Async: MonadThrow: ToFuture](
      databaseClient: DatabaseClientAlg[F, Source[*, NotUsed]],
      queryTimeout:   FiniteDuration
    )(implicit
      materializer: Materializer
    ): BlocksQuery =
      (in: QueryBlocksReq) =>
        (for {
          // query transactions as a value of Source
          blocksSourceResult <-
            databaseClient
              .queryBlocks(
                in.filter.getOrElse(
                  BlockFilter.of(BlockFilter.FilterType.All(BlockFilter.AllFilter()))
                )
              )
              .map(_.asRight[QueryBlocksRes.Failure.Reason])
              // handle a Mongo failure as a data store connection error
              .handleError(error => QueryBlocksRes.Failure.Reason.DataStoreConnectionError(error.getMessage).asLeft)
          queryResult <-
            // collect the source to a Success result unless we hit the configured timeout
            blocksSourceResult
              .map(source =>
                source
                  .collectWithTimeout(queryTimeout)
                  .map(QueryBlocksRes.Success.of)
                  .map(_.asRight[QueryBlocksRes.Failure.Reason])
                  .handleError(err => QueryBlocksRes.Failure.Reason.QueryTimeout(err.getMessage).asLeft)
              )
              // if we failed to create a source, return a failure
              .valueOr(err => err.asLeft.pure[F])
          result =
            // create the query response message to send to the client
            queryResult
              .map(success => QueryBlocksRes(QueryBlocksRes.Result.Success(success)))
              .valueOr(failure => QueryBlocksRes(QueryBlocksRes.Result.Failure(QueryBlocksRes.Failure(failure))))
        } yield result)
          // map the Async F functor to a `Future` which is required for Akka gRPC
          .mapFunctor[Future]
  }

  object Mock {

    def make: BlocksQuery =
      (_: QueryBlocksReq) =>
        Future.successful(
          QueryBlocksRes(
            QueryBlocksRes.Result.Success(
              QueryBlocksRes.Success(
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
          )
        )
  }
}
