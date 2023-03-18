package co.topl.genusLibrary.interpreter

import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.genusLibrary.algebras.HeaderInserter
import co.topl.genusLibrary.failure.{Failure, Failures}
import co.topl.genusLibrary.model.BlockData
import co.topl.genusLibrary.orientDb.schema.VertexSchemaInstances.instances._
import com.tinkerpop.blueprints.impls.orient.OrientGraph
import scala.jdk.CollectionConverters._
import scala.util.Try

object GraphHeaderInserter {

  def make[F[_]: Async](graph: OrientGraph): Resource[F, HeaderInserter[F]] =
    Resource.pure(
      new HeaderInserter[F] {

        override def insert(block: BlockData): F[Either[Failure, Unit]] = {

          val prop: java.util.Map[String, Object] = blockHeaderSchema.encode(block.header).asJava

          if (block.header.height == 1) {
            Async[F].delay {
              Try {
                graph.addVertex(s"class:${blockHeaderSchema.name}", prop)
                graph.commit()
              }.toEither
                .leftMap { th =>
                  graph.rollback()
                  Failures.FailureMessage(th.getMessage).asInstanceOf[Failure]
                }
            }
          } else {
            Async[F].delay {
              Try {
                val vout = graph.addVertex(s"class:${blockHeaderSchema.name}", prop)
                val vin = graph.getVertices("blockId", block.header.parentHeaderId.value.toByteArray).iterator().next()
                graph.addEdge("class:BlockHeaderEdge", vout, vin, "parent")
                graph.commit()
              }.toEither
                .leftMap { th =>
                  graph.rollback()
                  Failures.FailureMessage(th.getMessage).asInstanceOf[Failure]
                }
            }
          }
        }
      }
    )

}
