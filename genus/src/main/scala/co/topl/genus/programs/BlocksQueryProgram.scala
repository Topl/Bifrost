package co.topl.genus.programs

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.effect.kernel.Async
import cats.implicits._
import cats.~>
import co.topl.genus.algebras.QueryServiceAlg
import co.topl.genus.filters.BlockFilter
import co.topl.genus.ops.implicits._
import co.topl.genus.services.blocks_query._
import co.topl.genus.types.Block
import org.mongodb.scala.bson.conversions.Bson
import co.topl.genus.typeclasses.implicits._

import scala.concurrent.Future

object BlocksQueryProgram {

  object Eval {

    def make[F[_]: Async: *[_] ~> Future](
      queryService: QueryServiceAlg[F, Block, BlockFilter, Bson]
    ): BlocksQuery =
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

  object Mock {

    def make[F[_]]: BlocksQuery = new BlocksQuery {

      override def query(in: QueryBlocksReq): Future[QueryBlocksRes] =
        Future.successful(
          QueryBlocksRes(QueryBlocksRes.Result.Success(QueryBlocksRes.Success(List(Block(id = "test-id")))))
        )

      override def queryStream(in: BlocksQueryStreamReq): Source[BlocksQueryStreamRes, NotUsed] =
        Source.single(BlocksQueryStreamRes(BlocksQueryStreamRes.Result.Block(Block(id = "test-id"))))
    }
  }
}
