package co.topl.catsakka

import akka.NotUsed
import akka.stream.scaladsl.{BroadcastHub, Keep, RunnableGraph, Source}
import cats.effect.Async
import cats.effect.kernel.{Resource, Sync}
import cats.effect.std.Queue
import cats.~>
import fs2.Stream
import fs2.concurrent.Topic
import fs2.interop.reactivestreams.StreamOps

import scala.language.implicitConversions

trait AsFS2StreamOps {

  implicit def streamAsStreamOps[F[_]: Async, T](stream: Stream[F, T]): StreamFS2Ops[F, T] =
    new StreamFS2Ops(stream)

}

class StreamFS2Ops[F[_], T](val stream: Stream[F, T]) extends AnyVal {

  def toAkkaSource(implicit asyncF: Async[F]): Resource[F, Source[T, NotUsed]] =
    stream.toUnicastPublisher.map(p => Source.fromPublisher(p.publisher))

  /**
   * Slow downstream consumers normally block upstream producers.  This method prevents this behavior by buffering
   * upstream elements, dropping old elements in the buffer as it becomes full.
   */
  def dropOldest(buffer: Int)(implicit asyncF: Async[F]): Stream[F, T] =
    Stream
      .eval(Queue.circularBuffer[F, Option[T]](buffer))
      .flatMap(queue =>
        Stream
          .fromQueueNoneTerminated(queue)
          .concurrently(stream.enqueueNoneTerminated(queue))
      )
}

trait AsFS2TopicOps {
  implicit def topicAsTopicOps[F[_]: Async, T](topic: Topic[F, T]): FS2TopicOps[F, T] = new FS2TopicOps(topic)
}

class FS2TopicOps[F[_], T](val topic: Topic[F, T]) extends AnyVal {

  def toAkkaBroadcastSource(implicit asyncF: Async[F], r: RunnableGraphToF[F]): Resource[F, Source[T, NotUsed]] =
    topic.subscribeUnbounded.toAkkaSource
      .flatMap(_.toMat(BroadcastHub.sink)(Keep.right).resource[F])

  def subscribeDropOldest(buffer: Int)(implicit asyncF: Async[F]): Stream[F, T] =
    topic.subscribeUnbounded.dropOldest(buffer)
}
