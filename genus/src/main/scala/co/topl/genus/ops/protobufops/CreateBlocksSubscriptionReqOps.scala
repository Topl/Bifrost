package co.topl.genus.ops.protobufops

import co.topl.genus.algebras.SubscriptionServiceAlg
import co.topl.genus.filters.BlockFilter
import co.topl.genus.services.blocks_subscription.CreateBlocksSubscriptionReq

import scala.language.implicitConversions

final class CreateBlocksSubscriptionReqOps(val value: CreateBlocksSubscriptionReq) extends AnyVal {

  def toRequest: SubscriptionServiceAlg.CreateRequest[BlockFilter, Long] =
    SubscriptionServiceAlg.CreateRequest(
      value.filter,
      Option.when(value.startHeight != 0)(value.startHeight),
      value.confirmationDepth
    )
}

object CreateBlocksSubscriptionReqOps {

  trait ToCreateBlocksSubscriptionReqOps {

    implicit def fromCreateBlocksSubscriptionReq(value: CreateBlocksSubscriptionReq): CreateBlocksSubscriptionReqOps =
      new CreateBlocksSubscriptionReqOps(value)
  }
}
