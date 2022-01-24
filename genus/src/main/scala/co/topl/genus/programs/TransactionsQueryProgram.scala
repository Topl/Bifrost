package co.topl.genus.programs

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.effect.kernel.Async
import cats.~>
import co.topl.genus.algebras.QueryServiceAlg
import co.topl.genus.algebras.QueryServiceAlg.{QueryFailures, QueryRequest}
import co.topl.genus.filters.TransactionFilter
import co.topl.genus.services.transactions_query._
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types._
import org.mongodb.scala.bson.conversions.Bson

import scala.concurrent.Future

object TransactionsQueryProgram {

  object Eval {

    def make[F[_]: Async: *[_] ~> Future](
      queryService: QueryServiceAlg[F, Transaction, TransactionFilter, Bson]
    ): TransactionsQuery =
      new TransactionsQuery {

        override def query(in: QueryTxsReq): Future[QueryTxsRes] =
          queryService
            .asList(QueryRequest(in.filter, None, in.pagingOptions, in.confirmationDepth))
            .map(txs => QueryTxsRes(QueryTxsRes.Result.Success(QueryTxsRes.Success(txs))))
            .valueOr {
              case QueryFailures.InvalidQuery(failures) =>
                QueryTxsRes(
                  QueryTxsRes.Result
                    .Failure(QueryTxsRes.Failure(QueryTxsRes.Failure.Reason.InvalidQuery(failures.show)))
                )
              case QueryFailures.DataStoreConnectionError(error) =>
                QueryTxsRes(
                  QueryTxsRes.Result
                    .Failure(QueryTxsRes.Failure(QueryTxsRes.Failure.Reason.DataStoreConnectionError(error)))
                )
              case QueryFailures.QueryTimeout(message) =>
                QueryTxsRes(
                  QueryTxsRes.Result
                    .Failure(QueryTxsRes.Failure(QueryTxsRes.Failure.Reason.QueryTimeout(message)))
                )
            }
            .mapFunctor

        override def queryStreamed(in: TxsQueryStreamReq): Source[TxsQueryStreamRes, NotUsed] =
          Source
            .futureSource(
              queryService
                .asSource(QueryRequest(in.filter, None, None, in.confirmationDepth))
                .map(source => source.map(tx => TxsQueryStreamRes(TxsQueryStreamRes.Result.Tx(tx))))
                .valueOr {
                  case QueryFailures.InvalidQuery(failures) =>
                    Source.single(
                      TxsQueryStreamRes(
                        TxsQueryStreamRes.Result
                          .Failure(
                            TxsQueryStreamRes.Failure(TxsQueryStreamRes.Failure.Reason.InvalidQuery(failures.show))
                          )
                      )
                    )
                  case QueryFailures.DataStoreConnectionError(error) =>
                    Source.single(
                      TxsQueryStreamRes(
                        TxsQueryStreamRes.Result
                          .Failure(
                            TxsQueryStreamRes.Failure(TxsQueryStreamRes.Failure.Reason.DataStoreConnectionError(error))
                          )
                      )
                    )
                  // this case should not happen since timeout can only occur on collection
                  case _: QueryFailures.QueryTimeout => Source.empty
                }
                .mapFunctor
            )
            .mapMaterializedValue(_ => NotUsed)
      }
  }

  object Mock {

    def make[F[_]]: TransactionsQuery = new TransactionsQuery {

      override def query(in: QueryTxsReq): Future[QueryTxsRes] =
        Future.successful(
          QueryTxsRes(QueryTxsRes.Result.Success(QueryTxsRes.Success(List(Transaction(txId = "test-id")))))
        )

      override def queryStreamed(in: TxsQueryStreamReq): Source[TxsQueryStreamRes, NotUsed] =
        Source.single(TxsQueryStreamRes(TxsQueryStreamRes.Result.Tx(Transaction(txId = "test-id"))))
    }
  }
}
