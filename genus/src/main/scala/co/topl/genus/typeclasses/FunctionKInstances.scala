package co.topl.genus.typeclasses

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.~>

import scala.concurrent.Future

trait FunctionKInstances {

  implicit def ioToFuture(implicit ioRuntime: IORuntime): IO ~> Future = new ~>[IO, Future] {
    override def apply[A](fa: IO[A]): Future[A] = fa.unsafeToFuture()
  }
}
