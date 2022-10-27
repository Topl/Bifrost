package co.topl.db.leveldb

import cats.Applicative
import cats.data.OptionT
import cats.effect.{Async, Resource, Sync}
import cats.implicits._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.algebras.Store
import co.topl.codecs.bytes.typeclasses.Persistable
import fs2.io.file._
import org.iq80.leveldb.{CompressionType, DB, Options}
import scodec.bits.ByteVector

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

        /**
         * Use the instance of the DB within a blocking F context
         */
        private def useDb[R](f: DB => R): F[R] =
          Sync[F].blocking(f(db))
      }
    }

  /**
   * Creates an instance of a DB from the given path
   */
  def makeDb[F[_]: Async](
    baseDirectory:   Path,
    createIfMissing: Boolean = true,
    paranoidChecks:  Boolean = true,
    blockSize:       Int = 4 * 1024 * 1024,
    cacheSize:       Long = 0,
    maxOpenFiles:    Int = 10,
    compressionType: CompressionType = CompressionType.SNAPPY
  ): Resource[F, DB] = {
    val options = new Options
    options.createIfMissing(createIfMissing)
    options.paranoidChecks(paranoidChecks)
    options.blockSize(blockSize)
    options.cacheSize(cacheSize)
    options.maxOpenFiles(maxOpenFiles)
    options.compressionType(compressionType)

    val dbF =
      Applicative[F].whenA(createIfMissing)(Files[F].createDirectories(baseDirectory)) >>
      Sync[F].blocking {
        org.iq80.leveldb.impl.Iq80DBFactory.factory.open(
          baseDirectory.toNioPath.toFile,
          options
        )
      }

    Resource.fromAutoCloseable(dbF)
  }
}
