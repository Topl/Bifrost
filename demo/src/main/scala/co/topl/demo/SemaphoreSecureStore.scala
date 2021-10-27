package co.topl.demo

import cats.Monad
import cats.data.{Chain, OptionT}
import cats.effect.kernel.{Async, Sync}
import cats.effect.std.Semaphore
import cats.implicits._
import co.topl.crypto.keyfile.{SecureBytes, SecureData, SecureStore}

import java.io.BufferedWriter
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.util.chaining._

/**
 * Implements a SecureStore using a Cats-Effect Semaphore to manage concurrency.  The SecureStore maps to a particular
 * disk directory path, and Java NIO is used for performing disk operations.  The store caches entries in-memory.  Upon
 * deleting an entry, the in-memory cached item is erased and evicted as well as the on-disk representation.
 */
class SemaphoreSecureStore[F[_]: Monad: Sync](baseDirectory: Path, semaphore: Semaphore[F]) extends SecureStore[F] {
  import scala.jdk.CollectionConverters._

  private var entries: Map[String, SecureBytes] = Map.empty

  def list: F[Chain[String]] = withSemaphore(
    Sync[F].delay(
      Chain
        .fromSeq(
          Files
            .list(baseDirectory)
            .iterator()
            .asScala
            .toSeq
        )
        .filter(Files.isRegularFile(_))
        .map(_.getFileName.toString)
    )
  )

  def write(data: SecureData): F[Unit] = withSemaphore {
    val path = Paths.get(baseDirectory.toString, data.name)
    deleteImpl(data.name) >>
    Sync[F].delay(entries += (data.name -> data.bytes)) >>
    Sync[F].defer(
      data.bytes
        .foldLeft[F, BufferedWriter](Files.newBufferedWriter(path))((writer, byte) => writer.tap(_.write(byte)))(
          writer => writer.close().pure[F]
        )
        .void
    )
  }

  def read(name: String): F[Option[SecureData]] =
    withSemaphore(
      Sync[F].defer(
        OptionT
          .fromOption[F](entries.get(name).map(SecureData(name, _)))
          .orElseF(
            Sync[F].defer {
              val path = Paths.get(baseDirectory.toString, name)
              if (Files.exists(path) && Files.isRegularFile(path)) {
                val secureData = SecureData(name, SecureBytes(Files.readAllBytes(path)))
                entries += (name -> secureData.bytes)
                secureData.some.pure[F]
              } else
                Option.empty[SecureData].pure[F]
            }
          )
          .value
      )
    )

  def delete(name: String): F[Unit] =
    withSemaphore(deleteImpl(name))

  private def deleteImpl(name: String): F[Unit] =
    Sync[F].delay {
      entries
        .get(name)
        .foreach { bytes =>
          entries -= name
          bytes.erase()
        }
      val path = Paths.get(baseDirectory.toString, name)
      if (Files.exists(path) && Files.isRegularFile(path)) {
        Files.write(path, Array.fill[Byte](Files.size(path).toInt)(0), StandardOpenOption.TRUNCATE_EXISTING)
        Files.delete(path)
      }
    }

  private def withSemaphore[Res](compute: F[Res]): F[Res] =
    semaphore.permit.surround(compute)
}

object SemaphoreSecureStore {

  object Eval {

    def make[F[_]: Async](basePath: Path): F[SemaphoreSecureStore[F]] =
      Semaphore[F](1).map(new SemaphoreSecureStore(basePath, _))
  }
}
