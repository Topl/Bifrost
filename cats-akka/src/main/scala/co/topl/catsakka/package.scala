package co.topl

import akka.actor.typed.ActorSystem
import akka.stream.scaladsl.RunnableGraph
import cats.arrow.FunctionK
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.~>

import scala.concurrent.Future

package object catsakka extends SourceOps with RunnableGraphOps with FlowOps with FOps with AsFS2StreamOps {

  type FToFuture[F[_]] = F ~> Future
  type RunnableGraphToF[F[_]] = RunnableGraph ~> F

  implicit def ioToFuture(implicit ioRuntime: IORuntime): IO ~> Future =
    FunctionK.liftFunction(_.unsafeToFuture())

  implicit def systemToRuntime(implicit system: ActorSystem[_]): IORuntime =
    AkkaCatsRuntime(system).runtime

  implicit def classicSystemToRuntime(implicit system: akka.actor.ActorSystem): IORuntime = {
    import akka.actor.typed.scaladsl.adapter._
    systemToRuntime(system.toTyped)
  }
}
