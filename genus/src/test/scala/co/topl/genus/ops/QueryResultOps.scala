package co.topl.genus.ops

import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.data.EitherT
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.genus.algebras.QueryService.QueryFailure

import scala.language.implicitConversions

class QueryResultOps[F[_], T, Mat](private val value: EitherT[F, QueryFailure, Source[T, Mat]]) extends AnyVal {

  def materializeToList(implicit
    materializer: Materializer,
    asyncF:       Async[F]
  ): EitherT[F, QueryFailure, List[T]] =
    value
      .flatMap(result =>
        EitherT.right(
          asyncF
            .fromFuture(
              asyncF
                .delay(
                  result.runWith(Sink.seq[T])
                )
            )
            .map(_.toList)
        )
      )
}

object QueryResultOps {

  trait ToOps {

    implicit def queryResultOpsFromValue[F[_], T, Mat](
      value: EitherT[F, QueryFailure, Source[T, Mat]]
    ): QueryResultOps[F, T, Mat] =
      new QueryResultOps(value)
  }

  object implicits extends ToOps
}
