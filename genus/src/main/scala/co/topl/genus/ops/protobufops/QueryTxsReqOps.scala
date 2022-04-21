package co.topl.genus.ops.protobufops

import co.topl.genus.algebras.QueryService.QueryRequest
import co.topl.genus.filters.TransactionFilter
import co.topl.genus.services.transactions_query.{QueryTxsReq, TransactionSorting}

import scala.language.implicitConversions

final class QueryTxsReqOps(val value: QueryTxsReq) extends AnyVal {

  def toQueryRequest: QueryRequest[TransactionFilter, TransactionSorting] =
    QueryRequest(
      value.filter.getOrElse(TransactionFilter(TransactionFilter.FilterType.Empty)),
      value.sorting.getOrElse(TransactionSorting(TransactionSorting.SortBy.Empty)),
      value.pagingOptions,
      value.confirmationDepth
    )
}

object QueryTxsReqOps {

  trait ToQueryTxsReqOps {

    implicit def fromQueryTxsReq(queryTxsReq: QueryTxsReq): QueryTxsReqOps =
      new QueryTxsReqOps(queryTxsReq)
  }
}
