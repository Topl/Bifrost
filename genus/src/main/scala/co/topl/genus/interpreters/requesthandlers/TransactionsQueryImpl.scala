package co.topl.genus.interpreters.requesthandlers

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.stream.scaladsl.Source
import cats.effect.kernel.Async
import cats.~>
import co.topl.genus.algebras.TransactionsQueryService
import co.topl.genus.ops.implicits._
import co.topl.genus.services.transactions_query._
import co.topl.genus.typeclasses.implicits._

import scala.concurrent.Future

object TransactionsQueryImpl {

  def make[F[_]: Async: *[_] ~> Future](
    queryService:    TransactionsQueryService[F]
  )(implicit system: ActorSystem): TransactionsQuery =
    new TransactionsQuery {

      override def query(in: QueryTxsReq): Future[QueryTxsRes] =
        queryService
          .asList(in.toQueryRequest)
          .fold(QueryTxsRes.fromQueryFailure, QueryTxsRes.fromTransactions[List])
          .mapFunctor

      override def queryStreamed(in: TxsQueryStreamReq): Source[TxsQueryStreamRes, NotUsed] =
        Source
          .futureSource(
            queryService
              .asSource(in.toQueryRequest)
              .fold(
                failure => Source.single(TxsQueryStreamRes.fromQueryFailure(failure)),
                TxsQueryStreamRes.fromTransactions[Source[*, NotUsed]]
              )
              .mapFunctor
          )
          .mapMaterializedValue(_ => NotUsed)
    }
}
