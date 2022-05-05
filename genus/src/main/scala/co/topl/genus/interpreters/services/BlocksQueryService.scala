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
import co.topl.genus.types.Block
import co.topl.utils.mongodb.DocumentDecoder
import co.topl.utils.mongodb.codecs._
import co.topl.utils.mongodb.models.BlockDataModel
import org.mongodb.scala.Document

object BlocksQueryService {

  def make[F[_]: Async](queries: MongoQuery[F])(implicit materializer: Materializer): QueryService[F, Block] =
    new Impl[F](queries)

  private class Impl[F[_]: Async](queries: MongoQuery[F])(implicit materializer: Materializer)
      extends QueryService[F, Block] {

    override def asList[Filter: MongoFilter, Sort: MongoSort](
      request: QueryService.QueryRequest[Filter, Sort]
    ): EitherT[F, QueryService.QueryFailure, List[Block]] =
      EitherT.right[QueryService.QueryFailure](
        Async[F]
          .fromFuture(
            queries
              .query(request.filter, request.sort, request.paging)
              .map(_.mapConcat(documentToBlock(_).toSeq))
              .map(_.runWith(Sink.seq[Block]))
          )
          .map(_.toList)
      )

    override def asSource[Filter: MongoFilter, Sort: MongoSort](
      request: QueryService.QueryRequest[Filter, Sort]
    ): EitherT[F, QueryService.QueryFailure, Source[Block, NotUsed]] =
      EitherT.right[QueryService.QueryFailure](
        queries
          .query(request.filter, request.sort, request.paging)
          .map(_.mapConcat(documentToBlock(_).toSeq))
      )

    def documentToBlock(document: Document): Either[String, Block] =
      DocumentDecoder[BlockDataModel].fromDocument(document).map(_.transformTo[Block])
  }
}
