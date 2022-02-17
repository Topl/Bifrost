package co.topl.interpreters

import cats.Monad
import cats.effect.{Clock, Ref, Sync}
import cats.implicits._
import co.topl.algebras.Store
import co.topl.codecs.bytes.typeclasses.Identifiable
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.models.TypedIdentifier
import co.topl.typeclasses.implicits._

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
