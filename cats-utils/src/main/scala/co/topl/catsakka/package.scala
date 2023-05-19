package co.topl

import cats.arrow.FunctionK
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.~>

import scala.concurrent.Future

package object catsutils extends FOps with AsFS2StreamOps {

  implicit def ioToFuture(implicit ioRuntime: IORuntime): IO ~> Future =
    FunctionK.liftFunction(_.unsafeToFuture())
}
