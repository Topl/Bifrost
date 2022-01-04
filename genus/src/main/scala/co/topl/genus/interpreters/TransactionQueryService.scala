package co.topl.genus.interpreters

import co.topl.genus.services.transaction_query.{QueryTxReq, QueryTxRes, TransactionQuery}
import co.topl.genus.types.Transaction

import scala.concurrent.Future

object TransactionQueryService {

  object Mock {

    def make: TransactionQuery =
      (_: QueryTxReq) =>
        Future.successful(
          QueryTxRes(
            transactions = List(
              Transaction(
                txId = "test-id-1",
                blockHeight = 5
              ),
              Transaction(
                txId = "test-id-2",
                blockHeight = 10
              )
            )
          )
        )
  }
}
