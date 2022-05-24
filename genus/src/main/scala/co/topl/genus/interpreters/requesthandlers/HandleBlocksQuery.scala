package co.topl.genus.interpreters.requesthandlers

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.Source
import cats.effect.Async
import cats.{~>, Monad}
import co.topl.genus.algebras.QueryService
import co.topl.genus.ops.implicits._
import co.topl.genus.services.blocks_query._
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.Block

import scala.concurrent.Future

object HandleBlocksQuery {

  def make[F[_]: Async: Monad: *[_] ~> Future](
    queries:         QueryService[F, Block]
  )(implicit system: ActorSystem): BlocksQuery =
    new BlocksQuery {

      override def query(in: QueryBlocksReq): Future[QueryBlocksRes] =
        queries
          .asList(in.toQueryRequest)
          .fold(QueryBlocksRes.fromQueryFailure, QueryBlocksRes.fromBlocks[List])
          .mapFunctor

      override def queryStream(in: BlocksQueryStreamReq): Source[BlocksQueryStreamRes, NotUsed] =
        Source
          .futureSource(
            queries
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
