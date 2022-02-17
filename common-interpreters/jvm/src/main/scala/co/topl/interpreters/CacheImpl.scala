package co.topl.interpreters

import cats.effect.IO
import co.topl.algebras.Cache

import scala.concurrent.duration.Duration

object CacheImpl {

  object Eval {

    def make[T](): IO[Cache[IO, T]] =
      scalacache.caffeine
        .CaffeineCache[IO, T]
        .map(caffeine =>
          new Cache[IO, T] {

            def cachingF(key: String)(ttl: Option[Duration])(f: IO[T]): IO[T] =
              caffeine.cachingF(key)(ttl)(f)

            def get(key: String): IO[Option[T]] =
              caffeine.get(key)

            def put(key: String)(value: T): IO[Unit] =
              caffeine.put(key)(value)
          }
        )
  }
}
