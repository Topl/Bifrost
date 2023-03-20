package co.topl.genusLibrary.orientDb

import cats.effect.{Resource, Sync}
import cats.effect.implicits.effectResourceOps
import cats.implicits._
import co.topl.genusLibrary.model.GenusExceptions
import com.tinkerpop.blueprints.impls.orient.OrientGraphFactory
import java.io.File
import org.typelevel.log4cats.Logger

object OrientDBFacade {

  def make[F[_]: Sync: Logger](directoryPath: String, user: String, password: String): Resource[F, OrientGraphFactory] =
    for {
      directory <- Sync[F].blocking(new File(directoryPath)).toResource
      _ <- Either
        .cond(
          directory.isDirectory || directory.mkdir(),
          Resource.unit[F],
          GenusExceptions.FailureMessage(
            s"${directory.getAbsolutePath} exists but is not a directory, or it can not be created."
          )
        )
        .leftMap(e => println(e.getMessage))
        .valueOr(_ => Resource.canceled)

      // Start Orient Db embedded server
      orientGraphFactory <- Resource.make[F, OrientGraphFactory](
        Sync[F].delay(
          new OrientGraphFactory(s"plocal:${directory.getAbsolutePath}", user, password)
        )
      )(factory => Sync[F].delay(factory.close()))

      // Create if not exits Metadata describing the schema used for the Genus graph in OrientDB
      _ <- GenusGraphMetadata.make(orientGraphFactory.getNoTx).toResource.void

    } yield orientGraphFactory
}
