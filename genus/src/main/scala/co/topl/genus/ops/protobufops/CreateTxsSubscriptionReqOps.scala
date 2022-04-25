package co.topl.genus.ops.protobufops

import cats.implicits._
import co.topl.genus.algebras.SubscriptionService
import co.topl.genus.filters.TransactionFilter
import co.topl.genus.services.transactions_subscription.CreateTxsSubscriptionReq
import co.topl.genus.types.BlockHeight

import scala.language.implicitConversions

final class CreateTxsSubscriptionReqOps(private val value: CreateTxsSubscriptionReq) extends AnyVal {

  def toRequest: SubscriptionService.CreateRequest[TransactionFilter] =
    SubscriptionService.CreateRequest(
      value.filter.getOrElse(TransactionFilter.defaultInstance),
      if (value.startHeight >= 1) BlockHeight(value.startHeight).some else None,
      value.confirmationDepth
    )
}

object CreateTxsSubscriptionReqOps {

  trait ToCreateTxsSubscriptionReqOps {

    implicit def fromCreateTxsSubscriptionReqOps(value: CreateTxsSubscriptionReq): CreateTxsSubscriptionReqOps =
      new CreateTxsSubscriptionReqOps(value)
  }
}
