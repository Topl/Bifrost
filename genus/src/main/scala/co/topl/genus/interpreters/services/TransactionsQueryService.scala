package co.topl.genus.interpreters.services

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.data.EitherT
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.genus.algebras.{MongoStore, QueryService}
import co.topl.genus.typeclasses.{MongoFilter, MongoSort}
import co.topl.genus.types.Transaction
import co.topl.genus.ops.implicits._
import co.topl.genus.typeclasses.implicits._

object TransactionsQueryService {

  def make[F[_]: Async](
    store:                 MongoStore[F]
  )(implicit materializer: Materializer): QueryService[F, Transaction] = new Impl[F](store)

  private class Impl[F[_]: Async](store: MongoStore[F])(implicit materializer: Materializer)
      extends QueryService[F, Transaction] {

    override def asList[Filter: MongoFilter, Sort: MongoSort](
      request: QueryService.QueryRequest[Filter, Sort]
    ): EitherT[F, QueryService.QueryFailure, List[Transaction]] =
      EitherT.right[QueryService.QueryFailure](
        Async[F]
          .fromFuture(
            store
              .getDocumentsWithPaging(request.filter.toBsonFilter.some, request.sort.toBsonSorting.some, request.paging)
              .map(_.mapConcat(documentToTransaction(_).toSeq))
              .map(_.runWith(Sink.seq[Transaction]))
          )
          .map(_.toList)
      )

    override def asSource[Filter: MongoFilter, Sort: MongoSort](
      request: QueryService.QueryRequest[Filter, Sort]
    ): EitherT[F, QueryService.QueryFailure, Source[Transaction, NotUsed]] =
      EitherT.right[QueryService.QueryFailure](
        store
          .getDocumentsWithPaging(request.filter.toBsonFilter.some, request.sort.toBsonSorting.some, request.paging)
          .map(_.mapConcat(documentToTransaction(_).toSeq))
      )

  }
}
