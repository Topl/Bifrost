package co.topl.genusLibrary.orientDb {

  import cats.effect.implicits.effectResourceOps
  import cats.effect.{Resource, Sync, SyncIO}
  import cats.implicits._
  import co.topl.genusLibrary.orientDb.schema.{EdgeSchema, VertexSchema}
  import co.topl.genusLibrary.orientDb.instances.VertexSchemaInstances.instances._
  import co.topl.genusLibrary.orientDb.schema.EdgeSchemaInstances._
  import com.orientechnologies.orient.core.db.ODatabaseDocumentInternal
  import com.orientechnologies.orient.core.metadata.schema.OClass
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

    private[orientDb] def make[F[_]: Sync: Logger](orientGraphFactory: OrientGraphFactory): Resource[F, Unit] =
      for {
        db <- Resource.make(Sync[F].blocking(orientGraphFactory.getDatabase))(db => Sync[F].delay(db.close()))

        _ <- createSchema(db, blockHeaderSchema).toResource
        _ <- createSchema(db, blockBodySchema).toResource
        _ <- createSchema(db, ioTransactionSchema).toResource
        _ <- createSchema(db, addressSchema).toResource
        _ <- createSchema(db, canonicalHeadSchema).toResource

        _ <- Seq(blockHeaderEdge, blockHeaderBodyEdge, blockHeaderTxIOEdge, addressTxIOEdge)
          .traverse(e => createEdgeSchema(db, e))
          .void
          .toResource

      } yield ()

    private[orientDb] def createSchema[F[_]: Sync: Logger](
      db:     ODatabaseDocumentInternal,
      schema: VertexSchema[_]
    ): F[Unit] =
      Sync[F]
        .blocking(Option(db.getClass(schema.name)))
        .flatMap {
          case Some(oClass) =>
            Logger[F].info(s"${oClass.getName} class found, schema remains equals")
          case _ =>
            Sync[F]
              .blocking(db.createClass(schema.name, "V"))
              .flatTap(oClass => createProps(schema, oClass))
              .flatTap(oClass => createIndices(schema, oClass))
              .flatMap(oClass => createLinks(db, schema, oClass))
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
          Try(vs.links.foreach(l => oClass.createProperty(l.propertyName, l.linkType, db.getClass(l.linkedClass))))
        )
        .to[F]
        .onError { case e => Logger[F].error(e)(s"Failed to create link on ${vs.name}") }
        .void

    private[orientDb] def createEdgeSchema[F[_]: Sync: Logger](
      db:   ODatabaseDocumentInternal,
      edge: EdgeSchema
    ) =
      Sync[F]
        .blocking(Option(db.getClass(edge.label)))
        .flatMap {
          case Some(oClass) =>
            Logger[F].info(s"${oClass.getName} class found, schema remains equals")
          case _ =>
            Sync[F]
              .blocking(db.createClass(edge.label, "E"))
              .void

        }
  }
}
