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

    /**
     * Creates an instance of the BlocksQuery algebra-adjacent interface.
     * When given a query request, the handler will query the underlying data-store, collect values
     * returned from the data-store until the configured query timeout, and wrap the resulting values
     * into a response type.
     *
     * TODO: this interpreter is a replica of the `TransactionsQueryService` interpreter,
     *        and the general structure can probably be extracted for both
     *
     * @param databaseClient the database client for retrieving blocks from some data store
     * @param queryTimeout the amount of time to wait before a query times out
     * @param materializer a materializer for Akka streams
     * @tparam F a functor with instances of `Async`, `MonadThrow`, and `ToFuture`
     * @return an instance of BlocksQuery
     */
    def make[F[_]: Async: MonadThrow: ToFuture](
      databaseClient: DatabaseClientAlg[F, Source[*, NotUsed]],
      queryTimeout:   FiniteDuration
    )(implicit
      materializer: Materializer
    ): BlocksQuery =
      (in: QueryBlocksReq) =>
        (for {
          // query blocks as a value of `Source[Block]`
          blocksSourceResult <-
            databaseClient
              .queryBlocks(
                // use provided filter or default to a filter which gets all values
                // TODO: return an error if no filter provided -> too many values to query
                in.filter.getOrElse(
                  BlockFilter.of(BlockFilter.FilterType.All(BlockFilter.AllFilter()))
                ),
                in.pagingOptions
              )
              .map(_.asRight[QueryBlocksRes.Failure.Reason])
              // handle a Mongo failure as a data store connection error
              .handleError(error => QueryBlocksRes.Failure.Reason.DataStoreConnectionError(error.getMessage).asLeft)
          queryResult <-
            // collect the source to a Success result unless we hit the configured timeout
            blocksSourceResult
              .map(source =>
                source
                  // collect the Source into a `Seq[Block]` with a possible timeout
                  .collectWithTimeout(queryTimeout)
                  // map successful collection into a Success response
                  .map(blocks => QueryBlocksRes.Success.of(blocks).asRight[QueryBlocksRes.Failure.Reason])
                  // an error here would be due to a query timeout with the collection
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
