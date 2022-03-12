package co.topl.genus.ops.protobufops

import co.topl.genus.algebras.QueryServiceAlg.QueryRequest
import co.topl.genus.filters.BlockFilter
import co.topl.genus.services.blocks_query.QueryBlocksReq
import org.mongodb.scala.bson.conversions.Bson

import scala.language.implicitConversions

final class QueryBlocksReqOps(val value: QueryBlocksReq) extends AnyVal {

  def toQueryRequest: QueryRequest[BlockFilter, Bson] =
    QueryRequest(value.filter, None, value.pagingOptions, value.confirmationDepth)
}

object QueryBlocksReqOps {

  trait ToQueryBlocksReqOps {

    implicit def fromQueryBlocksReq(queryBlocksReq: QueryBlocksReq): QueryBlocksReqOps =
      new QueryBlocksReqOps(queryBlocksReq)
  }
}
