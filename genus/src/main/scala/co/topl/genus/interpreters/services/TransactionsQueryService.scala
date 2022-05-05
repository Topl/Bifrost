package co.topl.genus.interpreters.services

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.data.EitherT
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.genus.algebras.{MongoQuery, QueryService}
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.typeclasses.{MongoFilter, MongoSort}
import co.topl.genus.types.Transaction
import co.topl.utils.mongodb.DocumentDecoder
import co.topl.utils.mongodb.codecs._
import co.topl.utils.mongodb.models.ConfirmedTransactionDataModel
import org.mongodb.scala.Document

object TransactionsQueryService {

  def make[F[_]: Async](
    queries:               MongoQuery[F]
  )(implicit materializer: Materializer): QueryService[F, Transaction] = new Impl[F](queries)

  private class Impl[F[_]: Async](queries: MongoQuery[F])(implicit materializer: Materializer)
      extends QueryService[F, Transaction] {

    override def asList[Filter: MongoFilter, Sort: MongoSort](
      request: QueryService.QueryRequest[Filter, Sort]
    ): EitherT[F, QueryService.QueryFailure, List[Transaction]] =
      EitherT.right[QueryService.QueryFailure](
        Async[F]
          .fromFuture(
            queries
              .query(request.filter, request.sort, request.paging)
              .map(_.mapConcat(documentToTransaction(_).toSeq))
              .map(_.runWith(Sink.seq[Transaction]))
          )
          .map(_.toList)
      )

    override def asSource[Filter: MongoFilter, Sort: MongoSort](
      request: QueryService.QueryRequest[Filter, Sort]
    ): EitherT[F, QueryService.QueryFailure, Source[Transaction, NotUsed]] =
      EitherT.right[QueryService.QueryFailure](
        queries
          .query(request.filter, request.sort, request.paging)
          .map(_.mapConcat(documentToTransaction(_).toSeq))
      )

    def documentToTransaction(document: Document): Either[String, Transaction] =
      DocumentDecoder[ConfirmedTransactionDataModel].fromDocument(document).map(_.transformTo[Transaction])
  }
}
