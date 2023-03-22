package co.topl.genusLibrary.interpreter

import cats.data.EitherT
import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.consensus.models.{BlockHeader, BlockId}
import co.topl.genusLibrary.algebras.VertexFetcherAlgebra
import co.topl.genusLibrary.model.{GenusException, GenusExceptions}
import co.topl.genusLibrary.orientDb.schema.BlockHeaderVertexSchema.Field
import co.topl.genusLibrary.orientDb.schema.VertexSchemaInstances.instances._
import co.topl.node.models.BlockBody
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.orient.OrientGraphNoTx
import scala.jdk.CollectionConverters._
import scala.util.Try
import scodec.bits.ByteVector

object GraphVertexFetcher {

  def make[F[_]: Async](orientGraph: OrientGraphNoTx): Resource[F, VertexFetcherAlgebra[F]] =
    Resource.pure {
      new VertexFetcherAlgebra[F] {

        override def fetchHeader(blockId: BlockId): F[Either[GenusException, Option[BlockHeader]]] =
          EitherT(fetchHeaderVertex(blockId))
            .map(_.map(blockHeaderSchema.decodeVertex))
            .value

        override def fetchHeaderByHeight(height: Long): F[Either[GenusException, Option[BlockHeader]]] =
          Async[F].delay(
            Try(
              orientGraph.getVertices(blockHeaderSchema.name, Array(Field.Height), Array(height)).asScala
            ).toEither
              .map(_.headOption.map(blockHeaderSchema.decodeVertex))
              .leftMap[GenusException](tx => GenusExceptions.FailureMessageWithCause("FetchHeaderByHeight", tx))
          )

        /**
         * Fetch blockHeader using the index defined in the schema
         *
         * @param blockId used to fetch by blockid index
         * @return a header vertex
         */
        def fetchHeaderVertex(blockId: BlockId): F[Either[GenusException, Option[Vertex]]] =
          Async[F].delay(
            // Using BlockHeader index
            Try(orientGraph.getVertices(Field.BlockId, blockId.value.toByteArray).asScala).toEither
              .map(_.headOption)
              .leftMap[GenusException](tx => GenusExceptions.FailureMessageWithCause("FetchBodyVertex", tx))
          )

        /**
         * Fetch blockBody using the BlockBody link to BlockHeader defined in the schema
         *
         * @param headerVertex header db id will be used to fetch the body
         * @return a body vertex
         */
        def fetchBodyVertex(headerVertex: Vertex): F[Either[GenusException, Option[Vertex]]] =
          Async[F].delay(
            Try(
              orientGraph
                .getVertices(s"class:${blockBodySchema.name}", Array(Field.BlockId), Array(headerVertex.getId))
                .asScala
            ).toEither
              .map(_.headOption)
              .leftMap[GenusException](tx => GenusExceptions.FailureMessageWithCause("FetchBodyVertex", tx))
          )

        /**
         *  this method will be moved to the algebra
         * @param blockId
         * @return
         */
        def fetchBody(blockId: BlockId): F[Either[GenusException, Option[BlockBody]]] =
          (for {
            maybeHeaderVertex <- EitherT(fetchHeaderVertex(blockId))
            headerVertex <- EitherT.fromOption[F](
              maybeHeaderVertex,
              GenusExceptions.NoCurrentHeaderVertex(ByteVector(blockId.value.toByteArray)): GenusException
            )
            maybeBodyVertex <- EitherT(fetchBodyVertex(headerVertex))
            bodyVertex <- EitherT.fromOption[F](
              maybeBodyVertex,
              GenusExceptions.NoCurrentBodyVertex(ByteVector(blockId.value.toByteArray)): GenusException
            )
            blockBody <- EitherT.pure[F, GenusException](blockBodySchema.decodeVertex(bodyVertex).some)
          } yield (blockBody)).value

      }
    }
}
