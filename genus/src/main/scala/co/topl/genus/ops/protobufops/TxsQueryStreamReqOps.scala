package co.topl.genus.ops.protobufops

import co.topl.genus.algebras.QueryServiceAlg.QueryRequest
import co.topl.genus.filters.TransactionFilter
import co.topl.genus.services.transactions_query.TxsQueryStreamReq
import org.mongodb.scala.bson.conversions.Bson

import scala.language.implicitConversions

final class TxsQueryStreamReqOps(val value: TxsQueryStreamReq) extends AnyVal {

  def toQueryRequest: QueryRequest[TransactionFilter, Bson] =
    QueryRequest(value.filter, None, None, value.confirmationDepth)
}

object TxsQueryStreamReqOps {

  trait ToTxsQueryStreamReqOps {

    implicit def fromTxsQueryStreamReq(txsQueryStreamReq: TxsQueryStreamReq): TxsQueryStreamReqOps =
      new TxsQueryStreamReqOps(txsQueryStreamReq)
  }
}
