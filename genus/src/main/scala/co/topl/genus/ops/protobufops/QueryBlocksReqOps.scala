package co.topl.genus.ops.protobufops

import co.topl.genus.algebras.QueryService.QueryRequest
import co.topl.genus.filters.BlockFilter
import co.topl.genus.services.blocks_query.{BlockSorting, QueryBlocksReq}

import scala.language.implicitConversions

final class QueryBlocksReqOps(private val value: QueryBlocksReq) extends AnyVal {

  def toQueryRequest: QueryRequest[BlockFilter, BlockSorting] =
    QueryRequest(
      value.filter.getOrElse(BlockFilter(BlockFilter.FilterType.Empty)),
      value.sorting.getOrElse(BlockSorting(BlockSorting.SortBy.Empty)),
      value.pagingOptions,
      value.confirmationDepth
    )
}

object QueryBlocksReqOps {

  trait ToQueryBlocksReqOps {

    implicit def fromQueryBlocksReq(queryBlocksReq: QueryBlocksReq): QueryBlocksReqOps =
      new QueryBlocksReqOps(queryBlocksReq)
  }
}
