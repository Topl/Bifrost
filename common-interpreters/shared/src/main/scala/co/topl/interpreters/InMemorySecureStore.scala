package co.topl.interpreters

import cats.Monad
import cats.data.Chain
import cats.effect.kernel.{Concurrent, Ref}
import cats.implicits._
import co.topl.algebras.SecureStore
import co.topl.codecs.bytes.typeclasses.Persistable
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.models.Bytes

object InMemorySecureStore {

  object Eval {

    def make[F[_]: Monad: Concurrent]: F[SecureStore[F]] =
      Ref
        .of(Map.empty[String, Array[Byte]])
        .map(ref =>
          new SecureStore[F] {

            def write[A: Persistable](name: String, data: A): F[Unit] =
              for {
                _ <- erase(name)
                _ <- ref.update(m => m.updated(name, data.persistedBytes.toArray))
              } yield ()

            def consume[A: Persistable](name: String): F[Option[A]] =
              for {
                v <- ref.get.map(_.get(name).flatMap(Bytes(_).decodePersisted[A].toOption))
                _ <- erase(name)
              } yield v

            def list: F[Chain[String]] =
              ref.get.map(_.keys.toList).map(Chain.fromSeq)

            def erase(name: String): F[Unit] =
              ref.update(m =>
                m.updatedWith(name) { t =>
                  t.foreach { arr =>
                    arr.indices.foreach(arr(_) = 0)
                  }
                  None
                }
              )
          }
        )
  }
}
