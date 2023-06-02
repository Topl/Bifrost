package co.topl.genusLibrary.orientDb

import cats.effect.Async
import cats.effect.implicits.effectResourceOps
import cats.effect.{Resource, Sync}
import cats.implicits._
import co.topl.genusLibrary.model.GEs
import com.orientechnologies.orient.core.sql.OCommandSQL
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

  def make[F[_]: Async: Logger: Files: OrientThread](
    directoryPath: String,
    password:      String
  ): Resource[F, OrientGraphFactory] =
    for {
      directory   <- Resource.pure(Path(directoryPath))
      isDirectory <- Resource.eval(Files[F].isDirectory(directory))

      /**
       * If it is the first time we start orient-db server, we will create the directory, and access it with default credentials admin, admin
       * If orient-db server was restarted (we already change the password), we will not create the new directory, and access it with credentials admin, password-config-provided
       */
      (createDirRes, previousDb) <-
        if (isDirectory) Resource.pure[F, (Boolean, Boolean)]((true, true))
        else Resource.eval(Files[F].createDirectory(directory)).attempt.map(r => (r.isRight, false))

      _ <- Logger[F]
        .info(s"Genus directoryPath=$directoryPath previousDatabaseExists=$previousDb")
        .toResource

      _ <- Either
        .cond(
          createDirRes,
          Resource.unit[F],
          s"${directory.toNioPath} exists but is not a directory, or it can not be created."
        )
        .leftMap(cause =>
          Resource
            .eval(Logger[F].error(GEs.InternalMessage(cause).getMessage))
            .flatMap(_ => Resource.canceled)
        )
        .merge

      // Start Orient Db embedded server
      orientGraphFactory <- Resource.make[F, OrientGraphFactory](
        previousDb
          .pure[F]
          .ifM(
            OrientThread[F].delay(
              new OrientGraphFactory(s"plocal:${directory.toNioPath}", "admin", password)
            ),
            OrientThread[F].delay(
              new OrientGraphFactory(s"plocal:${directory.toNioPath}", "admin", "admin")
            )
          )
      )(factory => OrientThread[F].delay(factory.close()))

      _ <- OrientDBMetadataFactory.make[F](orientGraphFactory)

      // Change the default admin password
      _ <-
        previousDb
          .pure[F]
          .ifM(
            Sync[F].unit,
            OrientThread[F].delay {
              orientGraphFactory.getTx
                .command(
                  new OCommandSQL(s"UPDATE OUser SET password='$password' WHERE name='admin'")
                )
                .execute[Int]()
              orientGraphFactory.getTx.commit()
            }.void
          )
          .toResource

      // Once the password changed, return the new session
      orientGraphFactoryNewSession <- Resource.make[F, OrientGraphFactory](
        OrientThread[F].delay(
          new OrientGraphFactory(s"plocal:${directory.toNioPath}", "admin", password)
        )
      )(factory => OrientThread[F].delay(factory.close()))

    } yield orientGraphFactoryNewSession
}
