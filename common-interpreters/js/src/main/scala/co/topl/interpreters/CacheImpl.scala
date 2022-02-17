package co.topl.interpreters

import cats.effect.IO
import co.topl.algebras.Cache

import scala.concurrent.duration.Duration

object CacheImpl {

  object Eval {

    def make[T](): IO[Cache[IO, T]] =
      IO.delay(
        new Cache[IO, T] {

          def cachingF(key: String)(ttl: Option[Duration])(f: IO[T]): IO[T] =
            ???

          def get(key: String): IO[Option[T]] =
            ???

          def put(key: String)(value: T): IO[Unit] =
            ???
        }
      )
  }
}
