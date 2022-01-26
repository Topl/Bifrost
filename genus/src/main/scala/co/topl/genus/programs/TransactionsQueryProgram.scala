package co.topl.genus.programs

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.effect.kernel.Async
import cats.~>
import co.topl.genus.algebras.QueryServiceAlg
import co.topl.genus.algebras.QueryServiceAlg.QueryRequest
import co.topl.genus.filters.TransactionFilter
import co.topl.genus.ops.implicits._
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
            .fold(QueryTxsRes.fromQueryFailure, QueryTxsRes.fromTransactions[List])
            .mapFunctor

        override def queryStreamed(in: TxsQueryStreamReq): Source[TxsQueryStreamRes, NotUsed] =
          Source
            .futureSource(
              queryService
                .asSource(QueryRequest(in.filter, None, None, in.confirmationDepth))
                .fold(
                  failure => Source.single(TxsQueryStreamRes.fromQueryFailure(failure)),
                  TxsQueryStreamRes.fromTransactions[Source[*, NotUsed]]
                )
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
