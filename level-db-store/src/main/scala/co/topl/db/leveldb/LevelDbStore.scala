package co.topl.db.leveldb

import cats.Applicative
import cats.data.OptionT
import cats.effect.Async
import cats.effect.Resource
import cats.effect.Sync
import cats.implicits._
import cats.effect.implicits._
import co.topl.algebras.Store
import co.topl.codecs.bytes.typeclasses.Persistable
import co.topl.codecs.bytes.typeclasses.implicits._
import com.google.protobuf.ByteString
import fs2.io.file._
import org.iq80.leveldb.{Logger => _, _}
import org.typelevel.log4cats.Logger

import java.util.InputMismatchException

/**
 * A `Store` interpreter which is backed by LevelDB.  Keys and Values must have a Persistable typeclass instance available.
 * The keys and values are encoded to and from their persistable byte representation.
 */
object LevelDbStore {

  def make[F[_]: Sync, Key: Persistable, Value: Persistable](db: DB): F[Store[F, Key, Value]] =
    Sync[F].delay {
      new Store[F, Key, Value] {
        def put(id: Key, t: Value): F[Unit] =
          (Sync[F].delay(id.persistedBytes.toByteArray), Sync[F].delay(t.persistedBytes.toByteArray)).tupled
            .flatMap { case (idB, tB) => useDb(_.put(idB, tB)) }

        def remove(id: Key): F[Unit] =
          Sync[F].delay(id.persistedBytes.toByteArray).flatMap(idB => useDb(_.delete(idB)))

        def get(id: Key): F[Option[Value]] =
          Sync[F]
            .delay(id.persistedBytes.toByteArray)
            .flatMap(idB =>
              OptionT(useDb(db => Option(db.get(idB))))
                .semiflatMap(array =>
                  ByteString
                    .copyFrom(array)
                    .decodePersisted[Value]
                    .leftMap(new InputMismatchException(_))
                    .toEitherT[F]
                    .rethrowT
                )
                .value
            )

        def contains(id: Key): F[Boolean] =
          Sync[F]
            .delay(id.persistedBytes.toByteArray)
            .flatMap(idB => useDb(_.get(idB) != null))

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
    factory:         DBFactory,
    createIfMissing: Boolean = true,
    paranoidChecks:  Option[Boolean] = None,
    blockSize:       Option[Int] = None,
    cacheSize:       Option[Long] = None,
    maxOpenFiles:    Option[Int] = None,
    compressionType: Option[CompressionType] = None
  ): Resource[F, DB] = {
    val options = new Options
    options.createIfMissing(createIfMissing)
    paranoidChecks.foreach(options.paranoidChecks)
    blockSize.foreach(options.blockSize)
    cacheSize.foreach(options.cacheSize)
    maxOpenFiles.foreach(options.maxOpenFiles)
    compressionType.foreach(options.compressionType)

    val dbF =
      Applicative[F].whenA(createIfMissing)(Files.forAsync[F].createDirectories(baseDirectory)) >>
      Sync[F].blocking {
        factory.open(
          baseDirectory.toNioPath.toFile,
          options
        )
      }

    Resource.fromAutoCloseable(dbF)
  }

  private val nativeFactory = "org.fusesource.leveldbjni.JniDBFactory"
  private val javaFactory = "org.iq80.leveldb.impl.Iq80DBFactory"

  def makeFactory[F[_]: Sync: Logger]: Resource[F, DBFactory] =
    Sync[F]
      .delay(System.getProperty("os.name").toLowerCase().indexOf("mac") >= 0)
      // As LevelDB-JNI has problems on Mac (see https://github.com/ergoplatform/ergo/issues/1067),
      // we are using only pure-Java LevelDB on Mac
      .map(isMac => if (isMac) List(javaFactory) else List(nativeFactory, javaFactory))
      .flatMap(factories =>
        List(this.getClass.getClassLoader, ClassLoader.getSystemClassLoader)
          .zip(factories)
          .collectFirstSomeM { case (loader, factoryName) =>
            OptionT(
              Sync[F]
                .fromTry(
                  scala.util.Try(loader.loadClass(factoryName).getConstructor().newInstance().asInstanceOf[DBFactory])
                )
                .map(_.some)
                .recoverWith { case e =>
                  Logger[F].warn(e)(s"Failed to load database factory $factoryName").as(None)
                }
            ).map(factoryName -> _).value
          }
          .map(_.toRight(new RuntimeException(s"Could not load any of the factory classes: $factories")))
      )
      .rethrow
      .flatTap {
        case (`javaFactory`, _) =>
          Logger[F].warn("Using the pure java LevelDB implementation which is still experimental")
        case (name, factory) => Logger[F].info(s"Loaded $name with $factory")
      }
      .map(_._2)
      .toResource
}
