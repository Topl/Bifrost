package co.topl.genus.ops.protobufops

import cats.implicits._
import cats.Functor
import co.topl.genus.algebras.SubscriptionService.{CreateSubscriptionFailure, CreateSubscriptionFailures}
import co.topl.genus.services.transactions_subscription.TxsSubscriptionRes
import co.topl.genus.services.transactions_subscription.TxsSubscriptionRes.Failure.Reason
import co.topl.genus.types.Transaction

import scala.language.implicitConversions

final class TxsSubscriptionResCompanionOps(private val companion: TxsSubscriptionRes.type) extends AnyVal {

  def fromTransaction(transaction: Transaction): TxsSubscriptionRes =
    TxsSubscriptionRes(TxsSubscriptionRes.Result.Success(transaction))

  def fromTransactions[F[_]: Functor](transactions: F[Transaction]): F[TxsSubscriptionRes] =
    transactions.map(fromTransaction)

  def fromCreateFailure(failure: CreateSubscriptionFailure): TxsSubscriptionRes =
    fromFailureReason(
      failure match {
        case CreateSubscriptionFailures.DataConnectionFailure(message) => Reason.DataConnectionError(message)
        case CreateSubscriptionFailures.InvalidRequest(messages)       => Reason.InvalidRequest(messages.show)
      }
    )

  def fromFailureReason(reason: TxsSubscriptionRes.Failure.Reason): TxsSubscriptionRes =
    TxsSubscriptionRes(TxsSubscriptionRes.Result.Failure(TxsSubscriptionRes.Failure(reason)))
}

object TxsSubscriptionResCompanionOps {

  trait ToTxsSubscriptionResCompanionOps {

    implicit def fromCompanion(companion: TxsSubscriptionRes.type): TxsSubscriptionResCompanionOps =
      new TxsSubscriptionResCompanionOps(companion)
  }
}
