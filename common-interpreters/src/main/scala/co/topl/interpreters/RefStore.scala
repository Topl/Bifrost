package co.topl.interpreters

import cats.Monad
import cats.effect.{Clock, Ref, Sync}
import cats.implicits._
import co.topl.algebras.Store

object RefStore {

  object Eval {

    def make[F[_]: Monad: Clock: Sync, Key, T](): F[Store[F, Key, T]] =
      Ref
        .of[F, Map[Key, T]](Map.empty[Key, T])
        .map(ref =>
          new Store[F, Key, T] {

            def get(id: Key): F[Option[T]] =
              ref.get.map(_.get(id))

            def put(id: Key, t: T): F[Unit] =
              ref.update(_.updated(id, t))

            def remove(id: Key): F[Unit] =
              ref.update(_.removed(id))

            def contains(id: Key): F[Boolean] =
              ref.get.map(_.contains(id))
          }
        )
  }
}
