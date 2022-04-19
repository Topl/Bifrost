package co.topl.genus.ops.protobufops

import co.topl.genus.algebras.QueryService.QueryRequest
import co.topl.genus.filters.BlockFilter
import co.topl.genus.services.blocks_query.{BlockSorting, BlocksQueryStreamReq}

import scala.language.implicitConversions

final class BlocksQueryStreamReqOps(private val value: BlocksQueryStreamReq) extends AnyVal {

  def toQueryRequest: QueryRequest[BlockFilter, BlockSorting] =
    QueryRequest(
      value.filter.getOrElse(BlockFilter(BlockFilter.FilterType.Empty)),
      value.sorting.getOrElse(BlockSorting(BlockSorting.SortBy.Empty)),
      None,
      value.confirmationDepth
    )
}

object BlocksQueryStreamReqOps {

  trait ToBlocksQueryStreamReqOps {

    implicit def fromBlocksQueryStreamReq(blocksQueryStreamReq: BlocksQueryStreamReq): BlocksQueryStreamReqOps =
      new BlocksQueryStreamReqOps(blocksQueryStreamReq)
  }
}
