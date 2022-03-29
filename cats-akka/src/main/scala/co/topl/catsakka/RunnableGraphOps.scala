package co.topl.catsakka

import akka.event.Logging
import akka.stream.scaladsl.RunnableGraph
import akka.stream.{Attributes, Materializer}
import cats.arrow.FunctionK
import cats.effect.kernel.Sync
import cats.~>

import scala.language.implicitConversions

trait RunnableGraphOps {

  implicit def runnableGraphToF[F[_]: Sync](implicit mat: Materializer): RunnableGraph ~> F =
    FunctionK.liftFunction(rg => Sync[F].delay(rg.run()))

  implicit def runnableGraphAsRunnableGraphSupport[Mat](runnableGraph: RunnableGraph[Mat]): RunnableGraphSupport[Mat] =
    new RunnableGraphSupport[Mat](runnableGraph)
}

class RunnableGraphSupport[Mat](val runnableGraph: RunnableGraph[Mat]) extends AnyVal {

  def liftTo[F[_]: RunnableGraph ~> *[_]]: F[Mat] =
    implicitly[RunnableGraph ~> F].apply(runnableGraph)

  def withLogAttributes: RunnableGraph[Mat] =
    runnableGraph.withAttributes(Attributes.logLevels(onElement = Logging.InfoLevel, onFinish = Logging.InfoLevel))
}
