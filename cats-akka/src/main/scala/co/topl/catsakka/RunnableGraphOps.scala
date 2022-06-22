package co.topl.catsakka

import akka.event.Logging
import akka.stream.scaladsl.RunnableGraph
import akka.stream.{Attributes, Materializer}
import cats.arrow.FunctionK
import cats.effect.kernel.{Async, Sync}
import cats.~>

import scala.concurrent.Future
import scala.language.implicitConversions

trait RunnableGraphOps {

  implicit def runnableGraphToF[F[_]: Sync](implicit mat: Materializer): RunnableGraph ~> F =
    FunctionK.liftFunction(rg => Sync[F].delay(rg.run()))

  implicit def runnableGraphAsRunnableGraphSupport[Mat](runnableGraph: RunnableGraph[Mat]): RunnableGraphSupport[Mat] =
    new RunnableGraphSupport[Mat](runnableGraph)

  implicit def runnableGraphFutureAsRunnableGraphSupport[Mat](
    runnableGraph: RunnableGraph[Future[Mat]]
  ): RunnableGraphFutureSupport[Mat] =
    new RunnableGraphFutureSupport[Mat](runnableGraph)
}

class RunnableGraphSupport[Mat](val runnableGraph: RunnableGraph[Mat]) extends AnyVal {

  def liftTo[F[_]: RunnableGraph ~> *[_]]: F[Mat] =
    implicitly[RunnableGraph ~> F].apply(runnableGraph)

  def withLogAttributes: RunnableGraph[Mat] =
    runnableGraph.withAttributes(Attributes.logLevels(onElement = Logging.InfoLevel, onFinish = Logging.InfoLevel))
}

class RunnableGraphFutureSupport[Mat](val runnableGraph: RunnableGraph[Future[Mat]]) extends AnyVal {

  def liftFutureTo[F[_]: Async: RunnableGraph ~> *[_]]: F[Mat] =
    Async[F].fromFuture(implicitly[RunnableGraph ~> F].apply(runnableGraph))
}
