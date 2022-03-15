package co.topl

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{RunnableGraph, Source}
import cats.arrow.FunctionK
import cats.effect.kernel.Sync
import cats.kernel.Monoid
import cats.~>

import scala.concurrent.Future

package object demo {

  implicit def akkaSourceMonoid[T]: Monoid[Source[T, NotUsed]] =
    Monoid.instance[Source[T, NotUsed]](Source.empty, (s1, s2) => s1.merge(s2))

  implicit class SourceCatsOps[T, Mat](source: Source[T, Mat]) {

    def mapAsyncF[F[_]: *[_] ~> Future, U](parallelism: Int)(f: T => F[U]): Source[U, Mat] =
      source.map(f).mapAsync(parallelism)(implicitly[F ~> Future].apply)
  }

  implicit def runnableGraphToF[F[_]: Sync](implicit mat: Materializer): RunnableGraph ~> F =
    FunctionK.liftFunction(rg => Sync[F].delay(rg.run()))
}
