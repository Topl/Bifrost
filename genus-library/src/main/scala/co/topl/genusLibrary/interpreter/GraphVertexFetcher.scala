package co.topl.genusLibrary.interpreter

import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.consensus.models.{BlockHeader, BlockId}
import co.topl.genusLibrary.algebras.VertexFetcherAlgebra
import co.topl.genusLibrary.model.{GenusException, GenusExceptions}
import co.topl.genusLibrary.orientDb.schema.BlockHeaderVertexSchema.Field
import co.topl.genusLibrary.orientDb.schema.VertexSchemaInstances.instances._
import com.tinkerpop.blueprints.impls.orient.OrientGraphNoTx
import scala.jdk.CollectionConverters._
import scala.util.Try
import scodec.bits.ByteVector

object GraphVertexFetcher {

  def make[F[_]: Async](orientGraph: OrientGraphNoTx): Resource[F, VertexFetcherAlgebra[F]] =
    Resource.pure {
      new VertexFetcherAlgebra[F] {
        override def fetchHeader(blockId: BlockId): F[Either[GenusException, Option[BlockHeader]]] =
          Async[F].delay(
            Try(orientGraph.getVertices(Field.BlockId, blockId.value.toByteArray).asScala.headOption).toEither
              .map(_.map(blockHeaderSchema.decodeVertex))
              .leftMap[GenusException](_ =>
                GenusExceptions.NoCurrentHeaderVertex(ByteVector(blockId.value.toByteArray))
              )
          )

        override def fetchHeaderByHeight(height: Long): F[Either[GenusException, Option[BlockHeader]]] =
          Async[F].delay(
            Try(
              orientGraph.getVertices(blockHeaderSchema.name, Array(Field.Height), Array(height)).asScala.headOption
            ).toEither
              .map(_.map(blockHeaderSchema.decodeVertex))
              .leftMap[GenusException] { _ =>
                GenusExceptions.FailureMessage(s"Block header wasn't found for BlockId.height=[$height]")
              }
          )
      }
    }
}
