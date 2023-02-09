package co.topl.algebras

import cats.{Functor, MonadThrow, Show}
import cats.implicits._
import cats.data.OptionT

trait StoreReader[F[_], Key, T] {
  outer =>
  def get(id: Key): F[Option[T]]

  def contains(id: Key): F[Boolean]

  def getOrRaise(id: Key)(implicit monadThrow: MonadThrow[F], showKey: Show[Key]): F[T] =
    OptionT(get(id)).getOrElseF(monadThrow.raiseError(new NoSuchElementException(show"Element not found. id=$id")))

  def mapRead[KU, TU](fKey: KU => Key, fValue: T => TU)(implicit functor: Functor[F]): StoreReader[F, KU, TU] =
    new StoreReader[F, KU, TU] {
      def get(id: KU): F[Option[TU]] = OptionT(outer.get(fKey(id))).map(fValue).value

      def contains(id: KU): F[Boolean] = outer.contains(fKey(id))
    }
}

trait StoreWriter[F[_], Key, T] {
  def put(id:    Key, t: T): F[Unit]
  def remove(id: Key): F[Unit]
}

trait Store[F[_], Key, T] extends StoreReader[F, Key, T] with StoreWriter[F, Key, T]
