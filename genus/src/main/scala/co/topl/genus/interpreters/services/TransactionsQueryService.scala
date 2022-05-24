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

  def make[F[_]: Async: ChainHeight](store: MongoStore[F]): QueryService[F, Transaction] =
    new QueryService[F, Transaction] {

      override def query[Filter: MongoFilter: WithMaxBlockHeight, Sort: MongoSort](
        request: QueryRequest[Filter, Sort]
      ): EitherT[F, QueryFailure, Source[Transaction, NotUsed]] =
        EitherT.right[QueryFailure](
          for {
            filterWithConfirmationDepth <- request.filter.withConfirmationDepth[F](request.confirmationDepth)
            docsSource <-
              store.getDocumentsWithPaging(
                filterWithConfirmationDepth.toBsonFilter.some,
                request.sort.toBsonSorting.some,
                request.paging
              )
            blocks = docsSource.mapConcat(documentToTransaction(_).toSeq)
          } yield blocks
        )
    }
}
