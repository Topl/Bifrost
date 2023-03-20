package co.topl.genusLibrary.interpreter

import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.genusLibrary.algebras.HeaderInserter
import co.topl.genusLibrary.failure.{Failure, Failures}
import co.topl.genusLibrary.model.BlockData
import co.topl.genusLibrary.orientDb.schema.VertexSchemaInstances.instances._
import co.topl.genusLibrary.orientDb.schema.VertexSchemaBlockHeader.Field
import co.topl.genusLibrary.orientDb.schema.EdgeSchemaInstances._
import com.tinkerpop.blueprints.impls.orient.OrientGraph
import scala.jdk.CollectionConverters._
import scala.util.Try

object GraphBlockInserter {

  def make[F[_]: Async](graph: OrientGraph): Resource[F, HeaderInserter[F]] =
    Resource.pure(
      new HeaderInserter[F] {

        override def insert(block: BlockData): F[Either[Failure, Unit]] =
          Async[F].delay {

            // Genesis block
            if (block.header.height == 1) {
              Try {
                val headerVertex =
                  graph.addVertex(s"class:${blockHeaderSchema.name}", blockHeaderSchema.encode(block.header).asJava)

                val bodyVertex =
                  graph.addVertex(s"class:${blockBodySchema.name}", blockBodySchema.encode(block.body).asJava)

                graph.addEdge(s"class:${blockHeaderBodyEdgeSchema.name}", headerVertex, bodyVertex, "body")


                graph.commit()
              }.toEither
                .leftMap { th =>
                  graph.rollback()
                  Failures.FailureMessage(th.getMessage): Failure
                }
            } else {
              Try {
                val headerOutVertex =
                  graph.addVertex(s"class:${blockHeaderSchema.name}", blockHeaderSchema.encode(block.header).asJava)

                val bodyVertex =
                  graph.addVertex(s"class:${blockBodySchema.name}", blockBodySchema.encode(block.body).asJava)

                graph.addEdge(s"class:${blockHeaderBodyEdgeSchema.name}", headerOutVertex, bodyVertex, "body")

                // Lynk Parent Id
                val headerInVertex = graph
                  .getVertices(Field.BlockId, block.header.parentHeaderId.value.toByteArray)
                  .iterator()
                  .next()

                graph.addEdge(s"class:${blockHeaderEdgeSchema.name}", headerOutVertex, headerInVertex, "parent")

                graph.commit()
              }.toEither
                .leftMap { th =>
                  graph.rollback()
                  Failures.FailureMessage(th.getMessage): Failure
                }
            }
          }
      }
    )

}
