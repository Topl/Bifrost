package co.topl.genusLibrary.orientDb

import cats.effect.{Resource, Sync}
import cats.effect.implicits.effectResourceOps
import cats.implicits._
import co.topl.genusLibrary.model.GenusExceptions
import com.tinkerpop.blueprints.impls.orient.OrientGraphFactory
import java.io.File
import org.typelevel.log4cats.Logger

/**
 * DB Factory which has control over the following actions
 *
 * - Database file system validation and creation
 * - Create if not exist database metadata
 * - Be the entry point for Genus Service
 */
object OrientDBFactory {

  def make[F[_]: Sync: Logger](directoryPath: String, user: String, password: String): Resource[F, OrientGraphFactory] =
    for {
      directory <- Sync[F].delay(new File(directoryPath)).toResource
      continue  <- Sync[F].blocking(directory.isDirectory || directory.mkdir()).toResource
      _ <- Either
        .cond(
          continue,
          Resource.unit[F],
          GenusExceptions.FailureMessage(
            s"${directory.getAbsolutePath} exists but is not a directory, or it can not be created."
          )
        )
        .leftMap{e => Logger[F].error(e)("Process will end, change directory path")}
        .valueOr(_ => Resource.canceled)

      // Start Orient Db embedded server
      orientGraphFactory <- Resource.make[F, OrientGraphFactory](
        Sync[F].delay(
          new OrientGraphFactory(s"plocal:${directory.getAbsolutePath}", user, password)
        )
      )(factory => Sync[F].delay(factory.close()))

      // Create if not exits Metadata describing the schema used for the Genus graph in OrientDB
      _ <- OrientDBMetadataFactory.make(orientGraphFactory.getNoTx).toResource.void

    } yield orientGraphFactory
}
