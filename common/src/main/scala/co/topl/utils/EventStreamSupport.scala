package co.topl.utils

import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Sink, Source}
import akka.{Done, NotUsed}

import scala.concurrent.Future

object EventStreamSupport {

  def eventStreamSource[T](clazz: Class[T]): Source[T, NotUsed] =
    Source
      .fromMaterializer((mat, _) =>
        Source
          .actorRef(
            PartialFunction.empty,
            PartialFunction.empty,
            bufferSize = 256,
            overflowStrategy = OverflowStrategy.dropHead
          )
          .mapMaterializedValue { ref =>
            mat.system.eventStream.subscribe(ref, clazz)
            NotUsed
          }
      )
      .mapMaterializedValue(_ => NotUsed)

  def eventStreamSink: Sink[Any, Future[Done]] =
    Sink
      .fromMaterializer((mat, _) => Sink.foreach(mat.system.eventStream.publish))
      .mapMaterializedValue(_.flatten)
}
