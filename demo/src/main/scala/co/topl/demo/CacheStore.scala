package co.topl.demo

import cats.Monad
import cats.effect.{Clock, Ref, Sync}
import cats.implicits._
import co.topl.algebras.Store
import co.topl.codecs.bytes.typeclasses.Identifiable
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.models.TypedIdentifier
import co.topl.typeclasses.implicits._
import scalacache.caffeine.CaffeineCache

import scala.concurrent.duration.FiniteDuration

object CacheStore {

  object Eval {

    def make[F[_]: Monad: Clock: Sync, T: Identifiable](ttl: FiniteDuration): F[Store[F, T]] =
      CaffeineCache[F, T].map(implicit cache =>
        new Store[F, T] {

          def get(id: TypedIdentifier): F[Option[T]] =
            scalacache.get(id)

          def put(t: T): F[Unit] =
            scalacache.put(t.id)(t, ttl = Some(ttl))

          def remove(id: TypedIdentifier): F[Unit] =
            scalacache.remove(id)
        }
      )
  }
}

object RefStore {

  object Eval {

    def make[F[_]: Monad: Clock: Sync, T: Identifiable](): F[Store[F, T]] =
      Ref
        .of[F, Map[TypedIdentifier, T]](Map.empty[TypedIdentifier, T])
        .map(ref =>
          new Store[F, T] {

            def get(id: TypedIdentifier): F[Option[T]] =
              ref.get.map(_.get(id))

            def put(t: T): F[Unit] =
              ref.update(_.updated(t.id, t))

            def remove(id: TypedIdentifier): F[Unit] =
              ref.update(_.removed(id))
          }
        )
  }
}
