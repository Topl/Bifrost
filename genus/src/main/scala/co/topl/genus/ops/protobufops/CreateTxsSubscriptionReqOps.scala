package co.topl.genus.ops.protobufops

import co.topl.genus.algebras.SubscriptionServiceAlg
import co.topl.genus.filters.TransactionFilter
import co.topl.genus.services.transactions_subscription.CreateTxsSubscriptionReq

import scala.language.implicitConversions

final class CreateTxsSubscriptionReqOps(val value: CreateTxsSubscriptionReq) extends AnyVal {

  def toRequest: SubscriptionServiceAlg.CreateRequest[TransactionFilter, Long] =
    SubscriptionServiceAlg.CreateRequest(
      value.filter,
      Option.when(value.startHeight != 0)(value.startHeight),
      value.confirmationDepth
    )
}

object CreateTxsSubscriptionReqOps {

  trait ToCreateTxsSubscriptionReqOps {

    implicit def fromCreateTxsSubscriptionReqOps(value: CreateTxsSubscriptionReq): CreateTxsSubscriptionReqOps =
      new CreateTxsSubscriptionReqOps(value)
  }
}
