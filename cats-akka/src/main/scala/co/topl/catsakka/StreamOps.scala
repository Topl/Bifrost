package co.topl.catsakka

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.effect.Async
import fs2.Stream
import fs2.interop.reactivestreams.StreamOps
import scala.language.implicitConversions

trait FS2StreamOps {

  implicit def streamAsStreamOps[F[_]: Async, T](stream: Stream[F, T]): StreamFS2Ops[F, T] =
    new StreamFS2Ops(stream)

}

class StreamFS2Ops[F[_]: Async, T](val stream: Stream[F, T]) {

  def toAkkaSource: F[Source[T, NotUsed]] =
    stream.toUnicastPublisher.use { p =>
      Async[F].delay(Source.fromPublisher(p.publisher))
    }

}
