package co.topl.genus.programs

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.effect.kernel.Async
import cats.~>
import co.topl.genus.algebras.QueryServiceAlg
import co.topl.genus.algebras.QueryServiceAlg.{QueryFailures, QueryRequest}
import co.topl.genus.filters.BlockFilter
import co.topl.genus.services.blocks_query._
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.Block
import org.mongodb.scala.bson.conversions.Bson

import scala.concurrent.Future

object BlocksQueryProgram {

  object Eval {

    def make[F[_]: Async: *[_] ~> Future](
      queryService: QueryServiceAlg[F, Block, BlockFilter, Bson]
    ): BlocksQuery =
      new BlocksQuery {

        override def query(in: QueryBlocksReq): Future[QueryBlocksRes] =
          queryService
            .asList(QueryRequest(in.filter, None, in.pagingOptions, in.confirmationDepth))
            .map(blocks => QueryBlocksRes(QueryBlocksRes.Result.Success(QueryBlocksRes.Success(blocks))))
            .valueOr {
              case QueryFailures.InvalidQuery(failures) =>
                QueryBlocksRes(
                  QueryBlocksRes.Result
                    .Failure(QueryBlocksRes.Failure(QueryBlocksRes.Failure.Reason.InvalidQuery(failures.show)))
                )
              case QueryFailures.DataStoreConnectionError(error) =>
                QueryBlocksRes(
                  QueryBlocksRes.Result
                    .Failure(QueryBlocksRes.Failure(QueryBlocksRes.Failure.Reason.DataStoreConnectionError(error)))
                )
              case QueryFailures.QueryTimeout(message) =>
                QueryBlocksRes(
                  QueryBlocksRes.Result
                    .Failure(QueryBlocksRes.Failure(QueryBlocksRes.Failure.Reason.QueryTimeout(message)))
                )
            }
            .mapFunctor

        override def queryStream(in: BlocksQueryStreamReq): Source[BlocksQueryStreamRes, NotUsed] =
          Source
            .futureSource(
              queryService
                .asSource(QueryRequest(in.filter, None, None, in.confirmationDepth))
                .map(source => source.map(block => BlocksQueryStreamRes(BlocksQueryStreamRes.Result.Block(block))))
                .valueOr {
                  case QueryFailures.InvalidQuery(failures) =>
                    Source.single(
                      BlocksQueryStreamRes(
                        BlocksQueryStreamRes.Result
                          .Failure(
                            BlocksQueryStreamRes
                              .Failure(BlocksQueryStreamRes.Failure.Reason.InvalidQuery(failures.show))
                          )
                      )
                    )
                  case QueryFailures.DataStoreConnectionError(error) =>
                    Source.single(
                      BlocksQueryStreamRes(
                        BlocksQueryStreamRes.Result
                          .Failure(
                            BlocksQueryStreamRes
                              .Failure(BlocksQueryStreamRes.Failure.Reason.DataStoreConnectionError(error))
                          )
                      )
                    )
                  // this case should not happen since timeout can only occur on collection
                  case _: QueryFailures.QueryTimeout => Source.empty
                }
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
