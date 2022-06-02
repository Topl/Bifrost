package co.topl.genus.ops

import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.data.EitherT
import cats.effect.kernel.Async
import cats.implicits._

import scala.language.implicitConversions

class EitherTSourceOps[F[_], Left, T, Mat](private val value: EitherT[F, Left, Source[T, Mat]]) extends AnyVal {

  def materializeToList(implicit
    materializer: Materializer,
    asyncF:       Async[F]
  ): EitherT[F, Left, List[T]] =
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

object EitherTSourceOps {

  trait ToOps {

    implicit def eitherTSourceOpsFromValue[F[_], Left, T, Mat](
      value: EitherT[F, Left, Source[T, Mat]]
    ): EitherTSourceOps[F, Left, T, Mat] =
      new EitherTSourceOps(value)
  }

  object implicits extends ToOps
}
