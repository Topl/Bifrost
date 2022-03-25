package co.topl.catsakka

import cats.implicits._
import akka.stream.scaladsl.Flow
import cats.{~>, Functor}

import scala.concurrent.Future

trait FlowOps {

  implicit class FlowCatsOps[T, U, Mat](flow: Flow[T, U, Mat]) {

    def mapAsyncF[F[_]: *[_] ~> Future, U1](parallelism: Int)(f: U => F[U1]): Flow[T, U1, Mat] =
      flow.map(f).mapAsync(parallelism)(implicitly[F ~> Future].apply)

    def tapAsyncF[F[_]: Functor: *[_] ~> Future](parallelism: Int)(f: U => F[Unit]): Flow[T, U, Mat] =
      flow.map(a => f(a).as(a)).mapAsync(parallelism)(implicitly[F ~> Future].apply)
  }

}
