package co.topl.interpreters

import cats.Monad
import cats.effect.{Clock, Ref, Sync}
import cats.implicits._
import co.topl.algebras.Store
import co.topl.models.TypedIdentifier

object RefStore {

  object Eval {

    def make[F[_]: Monad: Clock: Sync, T](): F[Store[F, T]] =
      Ref
        .of[F, Map[TypedIdentifier, T]](Map.empty[TypedIdentifier, T])
        .map(ref =>
          new Store[F, T] {

            def get(id: TypedIdentifier): F[Option[T]] =
              ref.get.map(_.get(id))

            def put(id: TypedIdentifier, t: T): F[Unit] =
              ref.update(_.updated(id, t))

            def remove(id: TypedIdentifier): F[Unit] =
              ref.update(_.removed(id))

            def contains(id: TypedIdentifier): F[Boolean] =
              ref.get.map(_.contains(id))
          }
        )
  }
}
