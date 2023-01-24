package co.topl.catsakka

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.effect.Async
import cats.effect.kernel.Resource
import cats.effect.std.Queue
import fs2.Stream
import fs2.interop.reactivestreams.StreamOps

import scala.language.implicitConversions

trait AsFS2StreamOps {

  implicit def streamAsStreamOps[F[_], T](stream: Stream[F, T]): StreamFS2Ops[F, T] =
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
