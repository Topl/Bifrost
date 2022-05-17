package co.topl.genus.interpreters.services

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.data.EitherT
import cats.effect.kernel.Async
import co.topl.genus.algebras.{ChainHeight, MongoQuery, QueryService}
import co.topl.genus.typeclasses.{MongoFilter, MongoSort}
import co.topl.genus.types.Block

object BlocksQueryService {
  import QueryService._

  def make[F[_]: Async](queries: MongoQuery[F], chainHeight: ChainHeight[F]): QueryService[F, Block] =
    new Impl[F](queries, chainHeight)

  private class Impl[F[_]: Async](queries: MongoQuery[F], chainHeight: ChainHeight[F]) extends QueryService[F, Block] {

    override def query[Filter: MongoFilter, Sort: MongoSort](
      request: QueryService.QueryRequest[Filter, Sort]
    ): EitherT[F, QueryService.QueryFailure, Source[Block, NotUsed]] =
      for {
        currentHeight <- EitherT.right[QueryFailure](chainHeight.get)
        blocksDocsSource <- EitherT.right[QueryFailure](
          queries.query(request.filter, request.sort, request.paging)
        )
        blocksSource =
          blocksDocsSource
            .mapConcat(documentToBlock(_).toSeq)
            .filter(block => block.height <= (currentHeight.value - request.confirmationDepth))
      } yield blocksSource

  }
}
