package co.topl.typeclasses.crypto

import co.topl.models.Bytes

import java.nio.file.Path

trait Persistable[F[_], T] {
  def read(bytes: Bytes): F[T]
  def write(t:    T): F[Bytes]
}

trait Erasable[F[_], T] extends Persistable[F, T] {
  def erase(t: T, path: Path): F[T]
}
