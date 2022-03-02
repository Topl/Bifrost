package co.topl.genus.ops.protobufops

import cats.Functor
import cats.implicits._
import co.topl.genus.algebras.SubscriptionServiceAlg.{CreateSubscriptionFailure, CreateSubscriptionFailures}
import co.topl.genus.services.blocks_subscription.BlocksSubscriptionRes
import co.topl.genus.services.blocks_subscription.BlocksSubscriptionRes.Failure.Reason
import co.topl.genus.types.Block

import scala.language.implicitConversions

final class BlocksSubscriptionResCompanionOps(private val companion: BlocksSubscriptionRes.type) extends AnyVal {

  def fromBlocks[F[_]: Functor](blocks: F[Block]): F[BlocksSubscriptionRes] =
    blocks.map(fromBlock)

  def fromBlock(block: Block): BlocksSubscriptionRes =
    BlocksSubscriptionRes(BlocksSubscriptionRes.Result.Success(block))

  def fromCreateFailure(failure: CreateSubscriptionFailure): BlocksSubscriptionRes =
    fromFailureReason(
      failure match {
        case CreateSubscriptionFailures.DataConnectionFailure(message) => Reason.DataConnectionError(message)
        case CreateSubscriptionFailures.InvalidRequest(messages)       => Reason.InvalidRequest(messages.show)
      }
    )

  def fromFailureReason(reason: BlocksSubscriptionRes.Failure.Reason): BlocksSubscriptionRes =
    BlocksSubscriptionRes(BlocksSubscriptionRes.Result.Failure(BlocksSubscriptionRes.Failure(reason)))
}

object BlocksSubscriptionResCompanionOps {

  trait ToBlocksSubscriptionResCompanionOps {

    implicit def fromCompanion(companion: BlocksSubscriptionRes.type): BlocksSubscriptionResCompanionOps =
      new BlocksSubscriptionResCompanionOps(companion)
  }
}
