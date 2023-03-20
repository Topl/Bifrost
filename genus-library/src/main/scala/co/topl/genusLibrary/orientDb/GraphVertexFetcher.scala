package co.topl.genusLibrary.orientDb

import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.consensus.models.{BlockHeader, BlockId}
import co.topl.genusLibrary.model.{GenusException, GenusExceptions}
import co.topl.genusLibrary.orientDb.schema.VertexSchemaBlockHeader.Field
import co.topl.genusLibrary.orientDb.schema.VertexSchemaInstances.instances._
import com.tinkerpop.blueprints.impls.orient.OrientGraphNoTx
import org.typelevel.log4cats.Logger
import scala.jdk.CollectionConverters._
import scala.util.Try
import scodec.bits.ByteVector

object GraphVertexFetcher {

  def make[F[_]: Async: Logger](orientGraph: OrientGraphNoTx): Resource[F, VertexFetcher[F]] =
    Resource.pure {
      new VertexFetcher[F] {
        override def fetchHeader(blockId: BlockId): F[Either[GenusException, BlockHeader]] =
          Async[F].delay(
            Try(orientGraph.getVertices(Field.BlockId, blockId.value.toByteArray).asScala.head).toEither
              .map(blockHeaderSchema.decodeVertex)
              .leftMap[GenusException](_ =>
                GenusExceptions.NoCurrentHeaderVertex(ByteVector(blockId.value.toByteArray))
              )
          )

        override def fetchHeaderByHeight(height: Long): F[Either[GenusException, BlockHeader]] =
          Async[F].delay(
            Try(
              orientGraph.getVertices(blockHeaderSchema.name, Array(Field.Height), Array(height)).asScala.head
            ).toEither
              .map(blockHeaderSchema.decodeVertex)
              .leftMap[GenusException] { _ =>
                GenusExceptions.FailureMessage(s"Block header wasn't found for BlockId.height=[$height]")
              }
          )
      }
    }
}
