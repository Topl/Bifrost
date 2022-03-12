package co.topl.genus.ops.protobufops

import cats.implicits._
import cats.Functor
import co.topl.genus.algebras.QueryServiceAlg.{QueryFailure, QueryFailures}
import co.topl.genus.services.transactions_query.TxsQueryStreamRes
import co.topl.genus.services.transactions_query.TxsQueryStreamRes.Failure.Reason
import co.topl.genus.types.Transaction

import scala.language.implicitConversions

final class TxsQueryStreamResCompanionOps(val companion: TxsQueryStreamRes.type) extends AnyVal {

  def fromTransactions[F[_]: Functor](transactions: F[Transaction]): F[TxsQueryStreamRes] =
    transactions.map(fromTransaction)

  def fromTransaction(transaction: Transaction): TxsQueryStreamRes =
    TxsQueryStreamRes(TxsQueryStreamRes.Result.Tx(transaction))

  def fromQueryFailure(failure: QueryFailure): TxsQueryStreamRes =
    fromFailureReason(
      failure match {
        case QueryFailures.InvalidQuery(messages)            => Reason.InvalidQuery(messages.show)
        case QueryFailures.DataStoreConnectionError(message) => Reason.DataStoreConnectionError(message)
        case _                                               => Reason.Empty
      }
    )

  def fromFailureReason(reason: TxsQueryStreamRes.Failure.Reason): TxsQueryStreamRes =
    TxsQueryStreamRes(TxsQueryStreamRes.Result.Failure(TxsQueryStreamRes.Failure(reason)))
}

object TxsQueryStreamResCompanionOps {

  trait ToTxsQueryStreamResCompanionOps {

    implicit def fromTxsQueryStreamResCompanion(companion: TxsQueryStreamRes.type): TxsQueryStreamResCompanionOps =
      new TxsQueryStreamResCompanionOps(companion)
  }
}
