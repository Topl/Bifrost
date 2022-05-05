package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.Applicative
import cats.data.{Chain, EitherT}
import co.topl.genus.algebras.QueryService
import co.topl.genus.typeclasses.{MongoFilter, MongoSort}

object MockQueryService {

  def makeSuccessful[F[_]: Applicative, T](values: Chain[T]): QueryService[F, T] =
    new QueryService[F, T] {

      override def asList[Filter: MongoFilter, Sort: MongoSort](
        request: QueryService.QueryRequest[Filter, Sort]
      ): EitherT[F, QueryService.QueryFailure, List[T]] = EitherT.rightT[F, QueryService.QueryFailure](values.toList)

      override def asSource[Filter: MongoFilter, Sort: MongoSort](
        request: QueryService.QueryRequest[Filter, Sort]
      ): EitherT[F, QueryService.QueryFailure, Source[T, NotUsed]] =
        EitherT.rightT[F, QueryService.QueryFailure](Source(values.toList))
    }

  def makeFailing[F[_]: Applicative, T](failure: QueryService.QueryFailure): QueryService[F, T] =
    new QueryService[F, T] {

      override def asList[Filter: MongoFilter, Sort: MongoSort](
        request: QueryService.QueryRequest[Filter, Sort]
      ): EitherT[F, QueryService.QueryFailure, List[T]] = EitherT.leftT[F, List[T]](failure)

      override def asSource[Filter: MongoFilter, Sort: MongoSort](
        request: QueryService.QueryRequest[Filter, Sort]
      ): EitherT[F, QueryService.QueryFailure, Source[T, NotUsed]] = EitherT.leftT[F, Source[T, NotUsed]](failure)
    }
}
