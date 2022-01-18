package co.topl.genus.interpreters.queryservices

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import cats.MonadThrow
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.genus.algebras.DatabaseClientAlg
import co.topl.genus.filters.TransactionFilter
import co.topl.genus.interpreters.queryservices.implicits._
import co.topl.genus.services.transactions_query.{QueryTxsReq, QueryTxsRes, TransactionsQuery}
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.Transaction

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

object TransactionsQueryService {

  object Eval {

    /**
     * Creates an instance of the TransactionQuery algebra-adjacent interface.
     * When given a query request, the handler will query the underlying data-store, collect values
     * returned from the data-store until the configured query timeout, and wrap the resulting values
     * into a response type.
     *
     * @param databaseClient the database client for retrieving transactions from some data store
     * @param queryTimeout the amount of time to wait before a query times out
     * @param materializer a materializer for Akka streams
     * @tparam F a functor with instances of `Async`, `MonadThrow`, and `ToFuture`
     * @return an instance of TransactionQuery
     */
    def make[F[_]: Async: MonadThrow: ToFuture](
      databaseClient: DatabaseClientAlg[F, Source[*, NotUsed]],
      queryTimeout:   FiniteDuration
    )(implicit
      materializer: Materializer
    ): TransactionsQuery =
      (in: QueryTxsReq) =>
        (for {
          // query transactions as a value of `Source[Transaction]`
          transactionsSourceResult <-
            databaseClient
              .queryTransactions(
                // use provided filter or default to a filter which gets all values
                // TODO: return an error if no filter provided -> too many values to query
                in.filter.getOrElse(
                  TransactionFilter.of(TransactionFilter.FilterType.All(TransactionFilter.AllFilter()))
                ),
                in.pagingOptions
              )
              .map(_.asRight[QueryTxsRes.Failure.Reason])
              // handle a Mongo failure as a data store connection error
              .handleError(error => QueryTxsRes.Failure.Reason.DataStoreConnectionError(error.getMessage).asLeft)
          queryResult <-
            // collect the source to a Success result unless we hit the configured timeout
            transactionsSourceResult
              .map(source =>
                source
                  // collect the source into a `Seq[Transaction]` with a possible timeout
                  .collectWithTimeout(queryTimeout)
                  // map successful collection into a Success response
                  .map(txs => QueryTxsRes.Success.of(txs).asRight[QueryTxsRes.Failure.Reason])
                  // an error here would be due to a query timeout with the collection
                  .handleError(err => QueryTxsRes.Failure.Reason.QueryTimeout(err.getMessage).asLeft)
              )
              // if we failed to create a source, return a failure
              .valueOr(err => err.asLeft.pure[F])
          queryResponse =
            // create the query response message to send to the client
            queryResult
              .map(success => QueryTxsRes(QueryTxsRes.Result.Success(success)))
              .valueOr(failure => QueryTxsRes(QueryTxsRes.Result.Failure(QueryTxsRes.Failure(failure))))
        } yield queryResponse)
          // map the Async F functor to a `Future` which is required for Akka gRPC
          .mapFunctor[Future]
  }

  object Mock {

    def make: TransactionsQuery =
      (_: QueryTxsReq) =>
        Future.successful(
          QueryTxsRes(
            QueryTxsRes.Result.Success(
              QueryTxsRes.Success(
                List(
                  Transaction(
                    txId = "test-id-1",
                    blockHeight = 5
                  ),
                  Transaction(
                    txId = "test-id-2",
                    blockHeight = 10
                  )
                )
              )
            )
          )
        )
  }
}
