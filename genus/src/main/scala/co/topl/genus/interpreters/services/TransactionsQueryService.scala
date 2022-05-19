package co.topl.genus.interpreters.services

import cats.implicits._
import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.data.EitherT
import cats.effect.kernel.Async
import co.topl.genus.algebras._
import co.topl.genus.typeclasses._
import co.topl.genus.types.Transaction
import co.topl.genus.ops.implicits._
import co.topl.genus.typeclasses.implicits._

object TransactionsQueryService {

  import QueryService._

  def make[F[_]: Async](
    store:       MongoStore[F],
    chainHeight: ChainHeight[F]
  ): QueryService[F, Transaction] = new Impl[F](store, chainHeight)

  private class Impl[F[_]: Async](store: MongoStore[F], chainHeight: ChainHeight[F])
      extends QueryService[F, Transaction] {

    override def query[Filter: MongoFilter, Sort: MongoSort](
      request: QueryService.QueryRequest[Filter, Sort]
    ): EitherT[F, QueryService.QueryFailure, Source[Transaction, NotUsed]] =
      for {
        currentHeight <- EitherT.right[QueryFailure](chainHeight.get)
        transactionDocsSource <-
          EitherT.right[QueryFailure](
            store.getDocumentsWithPaging(
              request.filter.toBsonFilter.some,
              request.sort.toBsonSorting.some,
              request.paging
            )
          )
        transactionsSource =
          transactionDocsSource
            .mapConcat(documentToTransaction(_).toSeq)
            .filter(tx => tx.blockHeight <= (currentHeight.value - request.confirmationDepth))
      } yield transactionsSource

  }
}
