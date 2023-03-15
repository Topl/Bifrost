package co.topl.interpreters

import cats.implicits._
import cats.data.Chain
import cats.effect.std.Semaphore
import cats.effect.{Async, Resource, Sync}
import co.topl.algebras.SecureStore
import co.topl.codecs.bytes.typeclasses.Persistable
import co.topl.codecs.bytes.typeclasses.implicits._
import com.google.protobuf.ByteString

import scala.jdk.CollectionConverters._
import java.nio.file.{Files, Path, Paths, StandardOpenOption}

/**
 * Interprets the SecureStore algebra using cats-effect and java NIO.
 *
 * NOTE: While fs2-io _could_ be used in the implementation, this case benefits from more direct control
 * over the file system using java NIO
 */
object CatsSecureStore {

  def make[F[_]: Async](baseDir: Path): Resource[F, SecureStore[F]] =
    Resource.eval[F, SecureStore[F]](
      // Use a Semaphore to prevent multiple threads from operating on the Store
      Semaphore[F](1)
        .map(_.permit)
        .flatMap(permit =>
          Sync[F].delay(
            new SecureStore[F] {

              def write[A: Persistable](name: String, data: A): F[Unit] =
                usePermitBlocking(writeImpl(baseDir)(name)(data))

              def consume[A: Persistable](name: String): F[Option[A]] =
                usePermitBlocking(consumeImpl(baseDir)(name))

              def list: F[Chain[String]] =
                usePermitBlocking(listImpl(baseDir))

              def erase(name: String): F[Unit] =
                usePermitBlocking(eraseImpl(baseDir)(name))

              /**
               * Run the given thunk `f` inside of the Semaphore and within a blocking context
               */
              private def usePermitBlocking[Res](f: => Res): F[Res] =
                permit.use(_ => Sync[F].blocking(f))
            }
          )
        )
    )

  /**
   * Erase the secured data with the given name, or return unit if the data does not exist.
   */
  private[interpreters] def eraseImpl(baseDir: Path)(name: String): Unit = {
    val path = Paths.get(baseDir.toString, name)
    if (Files.exists(path) && Files.isRegularFile(path))
      eraseImplNoCheck(path)
  }

  /**
   * Erase the secured data at the exact given Path.  Overwrites the file with all 0-s, and then deletes
   * the file from disk.
   */
  private[interpreters] def eraseImplNoCheck(path: Path): Unit = {
    val size = Files.size(path).toInt
    Files.write(path, Array.fill[Byte](size)(0), StandardOpenOption.TRUNCATE_EXISTING)
    Files.delete(path)
  }

  /**
   * Saves data under the given name.  If data already exists at the given name, it is securely erased first.
   */
  private def writeImpl[A: Persistable](baseDir: Path)(name: String)(data: A): Unit = {
    eraseImpl(baseDir)(name)
    val path = Paths.get(baseDir.toString, name)
    val array = data.persistedBytes.toByteArray
    Files.write(path, array)
    array.indices.foreach(idx => array(idx) = 0: Byte)
  }

  /**
   * Consume data from disk.  Consumption also erases the data.
   */
  private def consumeImpl[A: Persistable](baseDir: Path)(name: String): Option[A] = {
    val path = Paths.get(baseDir.toString, name)
    if (Files.exists(path) && Files.isRegularFile(path)) {
      // Read file contents into memory
      val array = Files.readAllBytes(path)
      // Erase the file from disk
      eraseImplNoCheck(path)
      // Parse the bytes into `A`
      val result = ByteString.copyFrom(array).decodePersisted[A].toOption
      // Overwrite the byte array with 0s
      array.indices.foreach(idx => array(idx) = 0: Byte)
      // And return the parsed result
      result
    } else {
      None
    }
  }

  /**
   * Lists all secured data names under the given path
   */
  private def listImpl(baseDir: Path): Chain[String] =
    Chain
      .fromIterableOnce(
        Files
          .list(baseDir)
          .iterator()
          .asScala
      )
      .filter(Files.isRegularFile(_))
      .map(_.getFileName.toString)

}
