package co.topl.genus.interpreters.services

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.data.EitherT
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.genus.algebras._
import co.topl.genus.ops.implicits._
import co.topl.genus.typeclasses._
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.Block

object BlocksQueryService {
  import QueryService._

  def make[F[_]: Async: ChainHeight](store: MongoStore[F]): QueryService[F, Block] =
    new QueryService[F, Block] {

      override def query[Filter: MongoFilter: WithMaxBlockHeight, Sort: MongoSort](
        request: QueryRequest[Filter, Sort]
      ): EitherT[F, QueryFailure, Source[Block, NotUsed]] =
        EitherT.right[QueryFailure](
          for {
            filterWithConfirmationDepth <- request.filter.withConfirmationDepth[F](request.confirmationDepth)
            docsSource <-
              store.getDocumentsWithPaging(
                filterWithConfirmationDepth.toBsonFilter.some,
                request.sort.toBsonSorting.some,
                request.paging
              )
            blocks = docsSource.mapConcat(documentToBlock(_).toSeq)
          } yield blocks
        )
    }
}
