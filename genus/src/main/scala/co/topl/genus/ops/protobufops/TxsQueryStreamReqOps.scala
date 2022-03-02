package co.topl.genus.ops.protobufops

import co.topl.genus.algebras.QueryServiceAlg.QueryRequest
import co.topl.genus.filters.TransactionFilter
import co.topl.genus.services.transactions_query.{TransactionSorting, TxsQueryStreamReq}
import org.mongodb.scala.bson.conversions.Bson

import scala.language.implicitConversions

final class TxsQueryStreamReqOps(private val value: TxsQueryStreamReq) extends AnyVal {

  def toQueryRequest: QueryRequest[TransactionFilter, TransactionSorting] =
    QueryRequest(
      value.filter.getOrElse(TransactionFilter(TransactionFilter.FilterType.Empty)),
      value.sorting.getOrElse(TransactionSorting(TransactionSorting.SortBy.Empty)),
      None,
      value.confirmationDepth
    )
}

object TxsQueryStreamReqOps {

  trait ToTxsQueryStreamReqOps {

    implicit def fromTxsQueryStreamReq(txsQueryStreamReq: TxsQueryStreamReq): TxsQueryStreamReqOps =
      new TxsQueryStreamReqOps(txsQueryStreamReq)
  }
}
