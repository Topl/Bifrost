package co.topl.utils

import akka.event.EventStream
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Sink, Source}
import akka.{Done, NotUsed}
import spire.ClassTag

import scala.concurrent.Future

object EventStreamSupport {

  implicit class EventStreamSupp(eventStream: EventStream) {

    def source[T: ClassTag]: Source[T, NotUsed] =
      Source
        .actorRef(
          PartialFunction.empty,
          PartialFunction.empty,
          bufferSize = 256,
          overflowStrategy = OverflowStrategy.dropHead
        )
        .mapMaterializedValue { ref =>
          eventStream.subscribe(ref, implicitly[ClassTag[T]].runtimeClass)
          NotUsed
        }

    def sink: Sink[Any, Future[Done]] =
      Sink.foreach(eventStream.publish)
  }
}
