package co.topl.genus.ops.protobufops

import cats.Foldable
import cats.implicits._
import co.topl.genus.algebras.QueryServiceAlg.{QueryFailure, QueryFailures}
import co.topl.genus.services.blocks_query.QueryBlocksRes
import co.topl.genus.services.blocks_query.QueryBlocksRes.Failure.Reason
import co.topl.genus.types.Block

import scala.language.implicitConversions

final class QueryBlocksResCompanionOps(private val companion: QueryBlocksRes.type) extends AnyVal {

  def fromBlocks[F[_]: Foldable](blocks: F[Block]): QueryBlocksRes =
    QueryBlocksRes(QueryBlocksRes.Result.Success(QueryBlocksRes.Success(blocks.toList)))

  def fromQueryFailure(failure: QueryFailure): QueryBlocksRes =
    fromFailureReason(
      failure match {
        case QueryFailures.QueryTimeout(message)             => Reason.QueryTimeout(message)
        case QueryFailures.InvalidQuery(messages)            => Reason.InvalidQuery(messages.show)
        case QueryFailures.DataStoreConnectionError(message) => Reason.DataStoreConnectionError(message)
      }
    )

  def fromFailureReason(failure: QueryBlocksRes.Failure.Reason): QueryBlocksRes =
    QueryBlocksRes(QueryBlocksRes.Result.Failure(QueryBlocksRes.Failure(failure)))
}

object QueryBlocksResCompanionOps {

  trait ToQueryBlocksResCompanionExtensions {

    implicit def fromCompanion(companion: QueryBlocksRes.type): QueryBlocksResCompanionOps =
      new QueryBlocksResCompanionOps(companion)
  }
}
