package co.topl.genus.interpreters.requesthandlers

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.stream.scaladsl.Source
import cats.effect.Async
import cats.{~>, Monad}
import co.topl.genus.algebras.BlocksQueryService
import co.topl.genus.ops.implicits._
import co.topl.genus.services.blocks_query._
import co.topl.genus.typeclasses.implicits._

import scala.concurrent.Future

object BlocksQueryImpl {

  def make[F[_]: Async: Monad: *[_] ~> Future](
    queryService:    BlocksQueryService[F]
  )(implicit system: ActorSystem): BlocksQuery =
    new BlocksQuery {

      override def query(in: QueryBlocksReq): Future[QueryBlocksRes] =
        queryService
          .asList(in.toQueryRequest)
          .fold(QueryBlocksRes.fromQueryFailure, QueryBlocksRes.fromBlocks[List])
          .mapFunctor

      override def queryStream(in: BlocksQueryStreamReq): Source[BlocksQueryStreamRes, NotUsed] =
        Source
          .futureSource(
            queryService
              .asSource(in.toQueryRequest)
              .fold(
                failure => Source.single(BlocksQueryStreamRes.fromQueryFailure(failure)),
                BlocksQueryStreamRes.fromBlocks[Source[*, NotUsed]]
              )
              .mapFunctor
          )
          .mapMaterializedValue(_ => NotUsed)
    }
}
