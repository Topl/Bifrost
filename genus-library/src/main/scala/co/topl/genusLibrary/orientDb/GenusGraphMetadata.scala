package co.topl.genusLibrary.orientDb {

  import cats.effect.{Sync, SyncIO}
  import cats.implicits._
  import co.topl.genusLibrary.orientDb.schema.EdgeSchemaInstances.{blockHeaderBodyEdgeSchema, blockHeaderEdgeSchema}
  import co.topl.genusLibrary.orientDb.schema.VertexSchemaInstances.instances._
  import co.topl.genusLibrary.orientDb.schema.{EdgeSchema, VertexSchema}
  import com.tinkerpop.blueprints.impls.orient.OrientGraphNoTx
  import org.typelevel.log4cats.Logger
  import scala.util.Try

  object GenusGraphMetadata {

    def make[F[_]: Sync: Logger](graphNoTx: OrientGraphNoTx): F[Unit] =
      for {
        implicit0(g: OrientGraphNoTx) <- graphNoTx.pure[F]

        _ <- initializeVertex(blockHeaderSchema)
        _ <- initializeEdge(blockHeaderEdgeSchema)
        _ <- initializeVertex(blockBodySchema)
        _ <- initializeEdge(blockHeaderBodyEdgeSchema)

      } yield ()

    private def initializeVertex[F[_]: Sync: Logger](
      vertexSchema: VertexSchema[_]
    )(implicit graphNoTx: OrientGraphNoTx): F[Unit] =
      for {
        _ <- Logger[F].debug(s"${vertexSchema.name} schema vertex lookup")
        res <- SyncIO
          .fromOption(Option(graphNoTx.getVertexType(vertexSchema.name)))(
            new IllegalStateException(s"${vertexSchema.name} schema not found")
          )
          .handleErrorWith(_ =>
            SyncIO
              .fromTry(Try(graphNoTx.createVertexType(vertexSchema.name)))
              .map { vertexType =>
                vertexSchema.properties.foreach(property =>
                  property.propertyAttributeSetter(
                    vertexType.createProperty(property.name, property.propertyType)
                  )
                )
                vertexSchema.indices.foreach(index =>
                  vertexType.createIndex(index.name, index.indexType, index.propertyNames: _*)
                )
              }
          )
          .to[F]
          .onError { case e =>
            Logger[F].error(e)(s"Failed to create ${vertexSchema.name}")
          }
          .void
      } yield res

    private def initializeEdge[F[_]: Sync: Logger](
      edgeSchema: EdgeSchema
    )(implicit graphNoTx: OrientGraphNoTx): F[Unit] =
      for {
        _ <- Logger[F].debug(s"${edgeSchema.name} schema edge lookup")
        res <- SyncIO
          .fromOption(Option(graphNoTx.getEdgeType(edgeSchema.name)))(
            new IllegalStateException(s"${edgeSchema.name} schema not found")
          )
          .handleErrorWith(_ =>
            SyncIO
              .fromTry(Try(graphNoTx.createEdgeType(edgeSchema.name)))
          )
          .to[F]
          .onError { case e =>
            Logger[F].error(e)(s"Failed to create ${edgeSchema.name}")
          }
          .void
      } yield res
  }
}
