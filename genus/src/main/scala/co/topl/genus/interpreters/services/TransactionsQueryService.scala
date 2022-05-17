package co.topl.genus.interpreters.services

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.data.EitherT
import cats.effect.kernel.Async
import co.topl.genus.algebras.{ChainHeight, MongoQuery, QueryService}
import co.topl.genus.typeclasses.{MongoFilter, MongoSort}
import co.topl.genus.types.Transaction

object TransactionsQueryService {

  import QueryService._

  def make[F[_]: Async](
    queries:     MongoQuery[F],
    chainHeight: ChainHeight[F]
  ): QueryService[F, Transaction] = new Impl[F](queries, chainHeight)

  private class Impl[F[_]: Async](queries: MongoQuery[F], chainHeight: ChainHeight[F])
      extends QueryService[F, Transaction] {

    override def query[Filter: MongoFilter, Sort: MongoSort](
      request: QueryService.QueryRequest[Filter, Sort]
    ): EitherT[F, QueryService.QueryFailure, Source[Transaction, NotUsed]] =
      for {
        currentHeight <- EitherT.right[QueryFailure](chainHeight.get)
        transactionDocsSource <- EitherT.right[QueryFailure](
          queries.query(request.filter, request.sort, request.paging)
        )
        transactionsSource =
          transactionDocsSource
            .mapConcat(documentToTransaction(_).toSeq)
            .filter(tx => tx.blockHeight <= (currentHeight.value - request.confirmationDepth))
      } yield transactionsSource

  }
}
