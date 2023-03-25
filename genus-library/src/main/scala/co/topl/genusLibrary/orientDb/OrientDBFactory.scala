package co.topl.genusLibrary.orientDb

import cats.effect.{Resource, Sync}
import cats.implicits._
import co.topl.genusLibrary.model.GREs
import com.tinkerpop.blueprints.impls.orient.OrientGraphFactory
import fs2.io.file.{Files, Path}
import org.typelevel.log4cats.Logger

/**
 * DB Factory which has control over the following actions
 *
 * - Database file system creation
 *
 * - Create if not exist database metadata
 *
 * - Be the entry point for Genus Service
 */
object OrientDBFactory {

  def make[F[_]: Sync: Logger: Files](
    directoryPath: String,
    user:          String,
    password:      String
  ): Resource[F, OrientGraphFactory] =
    for {
      directory   <- Resource.pure(Path(directoryPath))
      isDirectory <- Resource.eval(Files[F].isDirectory(directory))
      createDirRes <-
        if (isDirectory) Resource.pure[F, Boolean](true)
        else Resource.eval(Files[F].createDirectory(directory)).attempt.map(_.isRight)

      _ <- Either
        .cond(
          createDirRes,
          Resource.unit[F],
          s"${directory.toNioPath} exists but is not a directory, or it can not be created."
        )
        .leftMap(cause =>
          Resource
            .eval(Logger[F].error(GREs.Message(cause).getMessage))
            .flatMap(_ => Resource.canceled)
        )
        .merge

      // Start Orient Db embedded server
      orientGraphFactory <- Resource.make[F, OrientGraphFactory](
        Sync[F].delay(
          new OrientGraphFactory(s"plocal:${directory.toNioPath}", user, password)
        )
      )(factory => Sync[F].delay(factory.close()))

      // If we need to recreate the schema from scratch backup, rename/move the db folder.
      _ <- OrientDBMetadataFactory.make(orientGraphFactory)

    } yield orientGraphFactory
}
