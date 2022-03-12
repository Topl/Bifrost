package co.topl.genus.ops.protobufops

import co.topl.genus.algebras.QueryServiceAlg.QueryRequest
import co.topl.genus.filters.BlockFilter
import co.topl.genus.services.blocks_query.BlocksQueryStreamReq
import org.mongodb.scala.bson.conversions.Bson

import scala.language.implicitConversions

final class BlocksQueryStreamReqOps(val value: BlocksQueryStreamReq) extends AnyVal {
  def toQueryRequest: QueryRequest[BlockFilter, Bson] = QueryRequest(value.filter, None, None, value.confirmationDepth)
}

object BlocksQueryStreamReqOps {

  trait ToBlocksQueryStreamReqOps {

    implicit def fromBlocksQueryStreamReq(blocksQueryStreamReq: BlocksQueryStreamReq): BlocksQueryStreamReqOps =
      new BlocksQueryStreamReqOps(blocksQueryStreamReq)
  }
}
