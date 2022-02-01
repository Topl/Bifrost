package co.topl.genus.ops.protobufops

import cats.implicits._
import co.topl.genus.algebras.SubscriptionServiceAlg
import co.topl.genus.filters.BlockFilter
import co.topl.genus.services.blocks_subscription.CreateBlocksSubscriptionReq
import co.topl.genus.types.BlockHeight

import scala.language.implicitConversions

final class CreateBlocksSubscriptionReqOps(val value: CreateBlocksSubscriptionReq) extends AnyVal {

  def toRequest: SubscriptionServiceAlg.CreateRequest[BlockFilter] =
    SubscriptionServiceAlg.CreateRequest(
      value.filter,
      if (value.startHeight >= 1) BlockHeight(value.startHeight).some else None,
      value.confirmationDepth
    )
}

object CreateBlocksSubscriptionReqOps {

  trait ToCreateBlocksSubscriptionReqOps {

    implicit def fromCreateBlocksSubscriptionReq(value: CreateBlocksSubscriptionReq): CreateBlocksSubscriptionReqOps =
      new CreateBlocksSubscriptionReqOps(value)
  }
}
