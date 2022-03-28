package co.topl.genus.ops.protobufops

import cats.implicits._
import cats.Functor
import co.topl.genus.algebras.QueryServiceAlg.{QueryFailure, QueryFailures}
import co.topl.genus.services.blocks_query.BlocksQueryStreamRes
import co.topl.genus.services.blocks_query.BlocksQueryStreamRes.Failure.Reason
import co.topl.genus.types.Block

import scala.language.implicitConversions

final class BlocksQueryStreamResCompanionOps(private val companion: BlocksQueryStreamRes.type) extends AnyVal {

  def fromBlock(block: Block): BlocksQueryStreamRes =
    BlocksQueryStreamRes(BlocksQueryStreamRes.Result.Block(block))

  def fromBlocks[F[_]: Functor](blocks: F[Block]): F[BlocksQueryStreamRes] =
    blocks.map(fromBlock)

  def fromQueryFailure(queryFailure: QueryFailure): BlocksQueryStreamRes =
    fromFailureReason(
      queryFailure match {
        case QueryFailures.InvalidQuery(messages)            => Reason.InvalidQuery(messages.show)
        case QueryFailures.DataStoreConnectionError(message) => Reason.DataStoreConnectionError(message)
        case _                                               => Reason.Empty
      }
    )

  def fromFailureReason(reason: BlocksQueryStreamRes.Failure.Reason): BlocksQueryStreamRes =
    BlocksQueryStreamRes(BlocksQueryStreamRes.Result.Failure(BlocksQueryStreamRes.Failure(reason)))
}

object BlocksQueryStreamResCompanionOps {

  trait ToBlocksQueryStreamResCompanionOps {

    implicit def fromCompanion(companion: BlocksQueryStreamRes.type): BlocksQueryStreamResCompanionOps =
      new BlocksQueryStreamResCompanionOps(companion)
  }
}
