package co.topl.db.leveldb

import cats.data.OptionT
import cats.effect.Sync
import cats.implicits._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.algebras.Store
import co.topl.codecs.bytes.typeclasses.Persistable
import org.iq80.leveldb.{CompressionType, DB, Options}
import scodec.bits.ByteVector

import java.nio.file.{Files, Path}
import java.util.InputMismatchException
import scala.language.implicitConversions

/**
 * A `Store` interpreter which is backed by LevelDB.  Keys and Values must have a Persistable typeclass instance available.
 * The keys and values are encoded to and from their persistable byte representation.
 */
object LevelDbStore {

  def make[F[_]: Sync, Key: Persistable, Value: Persistable](db: DB): F[Store[F, Key, Value]] =
    Sync[F].delay {
      new Store[F, Key, Value] {
        def put(id: Key, t: Value): F[Unit] =
          useDb(_.put(id.persistedBytes.toArray, t.persistedBytes.toArray))

        def remove(id: Key): F[Unit] =
          useDb(_.delete(id.persistedBytes.toArray))

        def get(id: Key): F[Option[Value]] =
          OptionT(useDb(db => Option(db.get(id.persistedBytes.toArray))))
            .semiflatMap(array =>
              ByteVector(array)
                .decodePersisted[Value]
                .leftMap(new InputMismatchException(_))
                .toEitherT[F]
                .rethrowT
            )
            .value

        def contains(id: Key): F[Boolean] =
          OptionT(useDb(db => Option(db.get(id.persistedBytes.toArray)))).isDefined

        private def useDb[R](f: DB => R): F[R] =
          Sync[F].blocking(f(db))
      }
    }

  /**
   * Creates an instance of a DB from the given path
   */
  def makeDb[F[_]: Sync](baseDirectory: Path): F[DB] =
    Sync[F].blocking {
      Files.createDirectories(baseDirectory)
      val options = new Options
      options.createIfMissing(true)
      options.paranoidChecks(true)
      options.blockSize(4 * 1024 * 1024)
      options.cacheSize(0)
      options.maxOpenFiles(10)
      options.compressionType(CompressionType.SNAPPY)
      org.iq80.leveldb.impl.Iq80DBFactory.factory.open(
        baseDirectory.toFile,
        options
      )
    }
}
