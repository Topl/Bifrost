package co.topl.genus.ops.protobufops

import co.topl.genus.algebras.QueryServiceAlg.QueryRequest
import co.topl.genus.filters.TransactionFilter
import co.topl.genus.services.transactions_query.QueryTxsReq
import org.mongodb.scala.bson.conversions.Bson

import scala.language.implicitConversions

final class QueryTxsReqOps(val value: QueryTxsReq) extends AnyVal {

  def toQueryRequest: QueryRequest[TransactionFilter, Bson] =
    QueryRequest(value.filter, None, value.pagingOptions, value.confirmationDepth)
}

object QueryTxsReqOps {

  trait ToQueryTxsReqOps {

    implicit def fromQueryTxsReq(queryTxsReq: QueryTxsReq): QueryTxsReqOps =
      new QueryTxsReqOps(queryTxsReq)
  }
}
