package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.genus.algebras.DatabaseClientAlg
import co.topl.genus.filters.TransactionFilter
import co.topl.genus.services.transaction_query.{QueryTxReq, QueryTxRes, TransactionQuery}
import co.topl.genus.types.Transaction

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

object TransactionQueryService {

  object Eval {

    def make(
      databaseClient: DatabaseClientAlg[IO, Source[*, NotUsed]],
      timeout:        FiniteDuration
    )(implicit
      executionContext: ExecutionContext,
      materializer:     Materializer
    ): TransactionQuery =
      (in: QueryTxReq) =>
        (for {
          transactionsSource <-
            databaseClient.queryTransactions(
              in.filter.getOrElse(TransactionFilter.of(TransactionFilter.FilterType.All(TransactionFilter.AllFilter())))
            )
          transactionsResult <- IO.fromFuture(transactionsSource.take(1000).runWith(Sink.seq).pure[IO])
        } yield QueryTxRes(transactionsResult))
          .timeout(timeout)
          .unsafeToFuture()
  }

  object Mock {

    def make: TransactionQuery =
      (_: QueryTxReq) =>
        Future.successful(
          QueryTxRes(
            transactions = List(
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
  }
}
