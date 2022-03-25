package co.topl.catsakka

import akka.event.Logging
import akka.stream.{Attributes, Materializer}
import akka.stream.scaladsl.RunnableGraph
import cats.arrow.FunctionK
import cats.effect.kernel.Sync
import cats.~>

trait RunnableGraphOps {

  implicit def runnableGraphToF[F[_]: Sync](implicit mat: Materializer): RunnableGraph ~> F =
    FunctionK.liftFunction(rg => Sync[F].delay(rg.run()))

  implicit class RunnableGraphSupport[Mat](runnableGraph: RunnableGraph[Mat]) {

    def mapK[G[_]: RunnableGraph ~> *[_]]: G[Mat] =
      implicitly[RunnableGraph ~> G].apply(runnableGraph)

    def withLogAttributes: RunnableGraph[Mat] =
      runnableGraph.withAttributes(Attributes.logLevels(onElement = Logging.InfoLevel, onFinish = Logging.InfoLevel))
  }
}
