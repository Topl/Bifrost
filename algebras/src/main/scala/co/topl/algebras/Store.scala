package co.topl.algebras

import cats.Functor
import cats.data.OptionT
import co.topl.models.TypedIdentifier

trait GenStoreReader[F[_], Key, T] {
  def get(id: Key): F[Option[T]]

  def contains(id: Key): F[Boolean]
}

trait StoreReader[F[_], T] extends GenStoreReader[F, TypedIdentifier, T]

trait GenStoreWriter[F[_], Key, T] {
  def put(id:    Key, t: T): F[Unit]
  def remove(id: Key): F[Unit]
}

trait StoreWriter[F[_], T] extends GenStoreWriter[F, TypedIdentifier, T]

trait GenStore[F[_], Key, T] extends GenStoreReader[F, Key, T] with GenStoreWriter[F, Key, T]
trait Store[F[_], T] extends GenStore[F, TypedIdentifier, T]
