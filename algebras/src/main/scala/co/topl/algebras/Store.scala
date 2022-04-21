package co.topl.algebras

import cats.Functor
import cats.data.OptionT
import co.topl.models.TypedIdentifier

trait StoreReader[F[_], T] {
  def get(id: TypedIdentifier): F[Option[T]]

  def mapRead[U](f: T => U)(implicit functor: Functor[F]): StoreReader[F, U] =
    id => OptionT(get(id)).map(f).value
}

trait StoreWriter[F[_], T] {
  o =>
  def put(id:    TypedIdentifier, t: T): F[Unit]
  def remove(id: TypedIdentifier): F[Unit]

  def mapWrite[U](f: U => T)(implicit functor: Functor[F]): StoreWriter[F, U] =
    new StoreWriter[F, U] {
      def put(id: TypedIdentifier, t: U): F[Unit] = o.put(id, f(t))

      def remove(id: TypedIdentifier): F[Unit] = o.remove(id)
    }
}

trait Store[F[_], T] extends StoreReader[F, T] with StoreWriter[F, T] {
  o =>

  def imapReadWrite[U](f: T => U)(g: U => T)(implicit functor: Functor[F]): Store[F, U] =
    new Store[F, U] {
      private val rMap = o.mapRead[U](f)
      private val wMap = o.mapWrite[U](g)
      def put(id: TypedIdentifier, t: U): F[Unit] = wMap.put(id, t)

      def remove(id: TypedIdentifier): F[Unit] = wMap.remove(id)

      def get(id: TypedIdentifier): F[Option[U]] = rMap.get(id)
    }
}
