package co.topl.genus.orientDb

import cats.effect.implicits.effectResourceOps
import cats.effect.{Async, Resource}
import cats.implicits._
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
      directory <- Resource.pure(Path(directoryPath))

      /**
       * If it is the first time we start orient-db server, we will create the directory, and access it with default credentials admin, admin
       * If orient-db server was restarted (we already change the password), we will not create the new directory, and access it with credentials admin, password-config-provided
       */
      exists <- Files[F]
        .exists(directory)
        .ifM(
          Files[F]
            .isDirectory(directory)
            .ifM(
              true.pure[F],
              Async[F].raiseError(new IllegalStateException("dataDir not a directory"))
            ),
          Files[F].createDirectories(directory).as(false)
        )
        .toResource
      _ <- Logger[F]
        .info(s"Genus directoryPath=$directoryPath previousDatabaseExists=$exists")
        .toResource

      // Start Orient Db embedded server
      orientGraphFactory <- Resource.make[F, OrientGraphFactory](
        if (exists)
          OrientThread[F].delay(
            new OrientGraphFactory(s"plocal:${directory.toNioPath}", "admin", password)
          )
        else
          OrientThread[F].delay(
            new OrientGraphFactory(s"plocal:${directory.toNioPath}", "admin", "admin")
          )
      )(factory => OrientThread[F].delay(factory.close()))

      _ <- OrientDBMetadataFactory.make[F](orientGraphFactory)

      // Change the default admin password
      _ <- Async[F]
        .unlessA(exists)(
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
