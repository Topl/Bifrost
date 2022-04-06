package co.topl.algebras.testInterpreters

import cats.implicits._
import cats.effect.kernel.{Async, Ref}
import co.topl.algebras.Store
import co.topl.models.TypedIdentifier

class TestStore[F[_]: Async, Key, T] extends Store[F, Key, T] {

  private val ref = Ref.of[F, Map[Key, T]](Map.empty[Key, T])

  def get(id: Key): F[Option[T]] =
    ref.flatMap(_.get.map(_.get(id)))

  def put(id: Key, t: T): F[Unit] =
    ref.flatMap(_.update(_.updated(id, t)))

  def remove(id: Key): F[Unit] =
    ref.flatMap(_.update(_.removed(id)))

  def contains(id: Key): F[Boolean] =
    ref.flatMap(_.get.map(_.contains(id)))
}
