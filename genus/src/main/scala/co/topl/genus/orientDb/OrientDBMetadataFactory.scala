package co.topl.genus.orientDb

import cats.effect.{Async, Resource, Sync}
import cats.implicits._
import co.topl.genus.orientDb.schema.{EdgeSchema, VertexSchema}
import co.topl.genus.orientDb.instances.VertexSchemaInstances.instances._
import co.topl.genus.orientDb.schema.EdgeSchemaInstances._
import com.orientechnologies.orient.core.db.ODatabaseDocumentInternal
import com.orientechnologies.orient.core.metadata.schema.{OClass, OType}
import com.tinkerpop.blueprints.impls.orient.OrientGraphFactory
import org.typelevel.log4cats.Logger

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
      db <- Resource
        .make(Sync[F].blocking(orientGraphFactory.getDatabase))(db =>
          OrientThread[F].delay {
            db.activateOnCurrentThread()
            db.close()
          }
        )
        .evalTap(db => OrientThread[F].delay(db.activateOnCurrentThread()).void)
      _ <- Resource.eval(
        OrientThread[F].defer(
          for {
            _ <- Seq(
              blockHeaderSchema,
              blockBodySchema,
              ioTransactionSchema,
              lockAddressSchema,
              txoSchema,
              groupPolicySchema,
              seriesPolicySchema
            )
              .traverse(createVertex(db, _))
              .void
          } yield ()
        )
      )
      _ <- Resource.eval(
        OrientThread[F].defer(
          for {
            _ <- Seq(
              blockHeaderEdge,
              blockHeaderBodyEdge,
              blockHeaderTxIOEdge,
              blockHeaderRewardEdge,
              addressTxIOEdge,
              addressTxoEdge
            )
              .traverse(e => createEdge(db, e))
              .void
          } yield ()
        )
      )
    } yield ()

  def createVertex[F[_]: Sync: Logger](db: ODatabaseDocumentInternal, schema: VertexSchema[_]): F[OClass] =
    // Even though the thread should already be active from the call up above, unit tests
    // may directly invoke this method without first initializing
    Sync[F].delay(db.activateOnCurrentThread()) >>
    Sync[F]
      .delay(Option(db.getClass(schema.name)).getOrElse(db.createClass(schema.name, "V")))
      .flatTap(oClass => createProps(schema, oClass))
      .flatTap(oClass => createLinks(db, schema, oClass))
      .flatTap(oClass => createIndices(schema, oClass))

  private def createProps[F[_]: Sync: Logger](vs: VertexSchema[_], oClass: OClass): F[Unit] =
    Sync[F]
      .catchNonFatal(
        vs.properties
          .filter(property => oClass.getProperty(property.name) == null)
          .foreach(property =>
            oClass
              .createProperty(property.name, property.propertyType)
              .setReadonly(property.readOnly)
              .setMandatory(property.mandatory)
              .setNotNull(property.notNull)
          )
      )
      .onError { case e => Logger[F].error(e)(s"Failed to create properties on ${vs.name}") }
      .void

  private def createIndices[F[_]: Sync: Logger](vs: VertexSchema[_], oClass: OClass): F[Unit] =
    Sync[F]
      .catchNonFatal {
        import scala.jdk.CollectionConverters._
        val currentIndices = oClass.getIndexes.asScala.map(_.getName)
        vs.indices
          .filterNot(i => currentIndices.contains(i.name))
          .foreach(i => oClass.createIndex(i.name, i.indexType, i.propertyNames: _*))
      }
      .onError { case e => Logger[F].error(e)(s"Failed to create indices on ${vs.name}") }
      .void

  private def createLinks[F[_]: Sync: Logger](
    db:     ODatabaseDocumentInternal,
    vs:     VertexSchema[_],
    oClass: OClass
  ): F[Unit] =
    Sync[F]
      .catchNonFatal(
        vs.links.foreach { l =>
          val property = Option(oClass.getProperty(l.propertyName))
            .getOrElse(oClass.createProperty(l.propertyName, OType.LINK))
          if (oClass.getProperty(l.propertyName).getLinkedClass == null)
            property.setLinkedClass(db.getClass(l.linkedClass))
        }
      )
      .onError { case e => Logger[F].error(e)(s"Failed to create link on ${vs.name}") }
      .void

  private[orientDb] def createEdge[F[_]: Sync: Logger](
    db:   ODatabaseDocumentInternal,
    edge: EdgeSchema
  ): F[Unit] =
    Sync[F].delay(db.activateOnCurrentThread()) >>
    Sync[F]
      .delay(Option(db.getClass(edge.label)))
      .flatMap {
        case Some(oClass) =>
          Logger[F].debug(s"${oClass.getName} class found, schema remains equals")
        case _ =>
          Sync[F].delay(db.createClass(edge.label, "E")).void
      }

}
