package co.topl.crypto.keyfile

import cats.{Defer, Monad}
import cats.implicits._
import cats.data.Chain

/**
 * A simple, in-memory, non-thread-safe implementation of a SecureStore
 *
 * (For testing purposes only)
 */
class EphemeralSecureStore[F[_]: Monad: Defer] extends SecureStore[F] {

  private var entries: Map[String, SecureBytes] = Map.empty

  def write(data: SecureData): F[Unit] =
    delete(data.name).map(_ => entries = entries.updated(data.name, data.bytes))

  def read(name: String): F[Option[SecureData]] =
    Defer[F].defer(entries.get(name).map(SecureData(name, _)).pure[F])

  def list: F[Chain[String]] =
    Defer[F].defer(Chain.fromSeq(entries.keys.toSeq).pure[F])

  def delete(name: String): F[Unit] =
    Defer[F].defer(
      entries
        .get(name)
        .foreach { bytes =>
          entries -= name
          bytes.erase()
        }
        .pure[F]
    )
}
