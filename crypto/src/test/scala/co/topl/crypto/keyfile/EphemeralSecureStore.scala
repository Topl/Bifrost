package co.topl.crypto.keyfile

import cats.data.Chain
import cats.implicits._
import cats.{Defer, Monad}
import co.topl.codecs.bytes.ByteCodec
import co.topl.codecs.bytes.ByteCodec.implicits._
import co.topl.models.Bytes

/**
 * A simple, in-memory, non-thread-safe implementation of a SecureStore
 *
 * (For testing purposes only)
 */
class EphemeralSecureStore[F[_]: Monad: Defer] extends SecureStore[F] {

  private var entries: Map[String, Bytes] = Map.empty

  def list: F[Chain[String]] =
    Defer[F].defer(Chain.fromSeq(entries.keys.toSeq).pure[F])

  def erase(name: String): F[Unit] =
    Defer[F].defer(
      entries
        .get(name)
        .foreach { bytes =>
          entries -= name
        }
        .pure[F]
    )

  def write[A: ByteCodec](name: String, data: A): F[Unit] =
    Defer[F].defer((entries += (name -> data.bytes)).pure[F])

  def consume[A: ByteCodec](name: String): F[Option[A]] =
    Defer[F].defer {
      {
        val entry = entries.get(name).map(_.decoded[A])
        entries -= name
        entry
      }.pure[F]
    }
}
