package co.topl.genus.ops.protobufops

import cats.Foldable
import cats.implicits._
import co.topl.genus.algebras.QueryServiceAlg.{QueryFailure, QueryFailures}
import co.topl.genus.services.transactions_query.QueryTxsRes
import co.topl.genus.services.transactions_query.QueryTxsRes.Failure.Reason
import co.topl.genus.types.Transaction

import scala.language.implicitConversions

final class QueryTxsResCompanionOps(val companion: QueryTxsRes.type) extends AnyVal {

  def fromTransactions[F[_]: Foldable](transactions: F[Transaction]): QueryTxsRes =
    QueryTxsRes(QueryTxsRes.Result.Success(QueryTxsRes.Success(transactions.toList)))

  def fromQueryFailure(queryFailure: QueryFailure): QueryTxsRes =
    fromFailureReason(
      queryFailure match {
        case QueryFailures.QueryTimeout(message)             => Reason.QueryTimeout(message)
        case QueryFailures.InvalidQuery(messages)            => Reason.InvalidQuery(messages.show)
        case QueryFailures.DataStoreConnectionError(message) => Reason.DataStoreConnectionError(message)
      }
    )

  def fromFailureReason(reason: QueryTxsRes.Failure.Reason): QueryTxsRes =
    QueryTxsRes(QueryTxsRes.Result.Failure(QueryTxsRes.Failure(reason)))
}

object QueryTxsResCompanionOps {

  trait ToQueryTxsResCompanionOps {

    implicit def fromQueryTxsResCompanion(companion: QueryTxsRes.type): QueryTxsResCompanionOps =
      new QueryTxsResCompanionOps(companion)
  }
}
