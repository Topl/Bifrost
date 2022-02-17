package co.topl.algebras

import co.topl.models.TypedIdentifier

trait Store[F[_], T] {
  def get(id:    TypedIdentifier): F[Option[T]]
  def put(t:     T): F[Unit]
  def remove(id: TypedIdentifier): F[Unit]
}
