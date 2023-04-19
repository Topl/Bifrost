package co.topl.genusLibrary.orientDb

import cats.effect.Async
import cats.effect.{Resource, Sync, SyncIO}
import cats.implicits._
import cats.effect.implicits._
import co.topl.genusLibrary.orientDb.schema.VertexSchema
import co.topl.genusLibrary.orientDb.instances.VertexSchemaInstances.instances._
import com.orientechnologies.orient.core.db.ODatabaseDocumentInternal
import com.orientechnologies.orient.core.metadata.schema.OClass
import com.orientechnologies.orient.core.metadata.schema.OType
import com.tinkerpop.blueprints.impls.orient.OrientGraphFactory
import org.typelevel.log4cats.Logger

import scala.util.Try

/**
 * Metadata Factory which has control over the following actions
 *
 * - Create vertices and edges for all schemas
 *
 * - Accessible on Orientdb scope, but not exposed to other packages of Genus Services
 */
object OrientDBMetadataFactory {

  private[orientDb] def make[F[_]: Async: Logger: OrientThread](
    orientGraphFactory: OrientGraphFactory
  ): Resource[F, Unit] =
    for {
      db <- Resource.make(Sync[F].blocking(orientGraphFactory.getDatabase))(db => Sync[F].delay(db.close()))
      _  <- OrientThread[F].delay(db.activateOnCurrentThread()).toResource
      _ <- Resource.eval(
        OrientThread[F].defer(
          for {
            _ <- createVertex(db, blockHeaderSchema)
            _ <- createVertex(db, blockBodySchema)
            _ <- createVertex(db, ioTransactionSchema)
            _ <- createVertex(db, canonicalHeadSchema)
          } yield ()
        )
      )
    } yield ()

  private[orientDb] def createVertex[F[_]: Sync: Logger](db: ODatabaseDocumentInternal, schema: VertexSchema[_]) =
    // Even though the thread should already be active from the call up above, unit tests
    // may directly invoke this method without first initializing
    Sync[F].delay(db.activateOnCurrentThread()) >>
    Sync[F]
      .delay(Option(db.getClass(schema.name)))
      .flatMap {
        case Some(oClass) =>
          Logger[F]
            .info(s"${oClass.getName} class found, schema remains equals")
            .as(oClass)
        case _ =>
          Sync[F]
            .delay(db.createClass(schema.name, "V"))
            .flatTap(oClass => createProps(schema, oClass))
            .flatTap(oClass => createIndices(schema, oClass))
            .flatTap(oClass => createLinks(db, schema, oClass))
      }

  private def createProps[F[_]: Sync: Logger](vs: VertexSchema[_], oClass: OClass): F[Unit] =
    SyncIO
      .fromTry(
        Try(
          vs.properties.foreach(property =>
            oClass
              .createProperty(property.name, property.propertyType)
              .setReadonly(property.readOnly)
              .setMandatory(property.mandatory)
              .setNotNull(property.notNull)
          )
        )
      )
      .to[F]
      .onError { case e => Logger[F].error(e)(s"Failed to create properties on ${vs.name}") }
      .void

  private def createIndices[F[_]: Sync: Logger](vs: VertexSchema[_], oClass: OClass): F[Unit] =
    SyncIO
      .fromTry(Try(vs.indices.foreach(i => oClass.createIndex(i.name, i.indexType, i.propertyNames: _*))))
      .to[F]
      .onError { case e => Logger[F].error(e)(s"Failed to create indices on ${vs.name}") }
      .void

  private def createLinks[F[_]: Sync: Logger](
    db:     ODatabaseDocumentInternal,
    vs:     VertexSchema[_],
    oClass: OClass
  ): F[Unit] =
    SyncIO
      .fromTry(
        Try(
          vs.links.foreach { l =>
            val property = Option(oClass.getProperty(l.propertyName))
              .getOrElse(oClass.createProperty(l.propertyName, OType.LINK))
            if (oClass.getProperty(l.propertyName).getLinkedClass == null)
              property.setLinkedClass(db.getClass(l.linkedClass))
          }
        )
      )
      .to[F]
      .onError { case e => Logger[F].error(e)(s"Failed to create link on ${vs.name}") }
      .void
}
