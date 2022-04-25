package co.topl.genus.ops.protobufops

import cats.implicits._
import co.topl.genus.algebras.SubscriptionService
import co.topl.genus.filters.BlockFilter
import co.topl.genus.services.blocks_subscription.CreateBlocksSubscriptionReq
import co.topl.genus.types.BlockHeight

import scala.language.implicitConversions

final class CreateBlocksSubscriptionReqOps(private val value: CreateBlocksSubscriptionReq) extends AnyVal {

  def toRequest: SubscriptionService.CreateRequest[BlockFilter] =
    SubscriptionService.CreateRequest(
      value.filter.getOrElse(BlockFilter.defaultInstance),
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
