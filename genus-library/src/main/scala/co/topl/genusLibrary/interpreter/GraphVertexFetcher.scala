package co.topl.genusLibrary.interpreter

import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.consensus.models.BlockId
import co.topl.genusLibrary.algebras.VertexFetcherAlgebra
import co.topl.genusLibrary.model.{GenusException, GenusExceptions}
import co.topl.genusLibrary.orientDb.schema.SchemaBlockHeader.Field
import co.topl.genusLibrary.orientDb.schema.VertexSchemaInstances.instances._
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.orient.OrientGraphNoTx
import scala.jdk.CollectionConverters._
import scala.util.Try

object GraphVertexFetcher {

  def make[F[_]: Async](orientGraph: OrientGraphNoTx): Resource[F, VertexFetcherAlgebra[F]] =
    Resource.pure {
      new VertexFetcherAlgebra[F] {

        override def fetchHeader(blockId: BlockId): F[Either[GenusException, Option[Vertex]]] =
          Async[F].delay(
            Try(orientGraph.getVertices(Field.BlockId, blockId.value.toByteArray).asScala).toEither
              .map(_.headOption)
              .leftMap[GenusException](tx => GenusExceptions.MessageWithCause("FetchBodyVertex", tx))
          )

        override def fetchBody(headerVertex: Vertex): F[Either[GenusException, Option[Vertex]]] =
          Async[F].delay(
            Try(
              orientGraph
                .getVertices(blockBodySchema.name, Array(Field.BlockId), Array(headerVertex.getId))
                .asScala
            ).toEither
              .map(_.headOption)
              .leftMap[GenusException](tx => GenusExceptions.MessageWithCause("FetchBodyVertex", tx))
          )

        override def fetchTransactions(headerVertex: Vertex): F[Either[GenusException, Iterable[Vertex]]] =
          Async[F].delay(
            Try(
              orientGraph
                .getVertices(ioTransactionSchema.name, Array(Field.BlockId), Array(headerVertex.getId))
                .asScala
            ).toEither
              .leftMap[GenusException](tx => GenusExceptions.MessageWithCause("FetchTransactionVertex", tx))
          )

        def fetchHeaderByHeight(height: Long): F[Either[GenusException, Option[Vertex]]] =
          Async[F].delay(
            Try(
              orientGraph.getVertices(blockHeaderSchema.name, Array(Field.Height), Array(height)).asScala
            ).toEither
              .map(_.headOption)
              .leftMap[GenusException](tx => GenusExceptions.MessageWithCause("FetchHeaderByHeightVertex", tx))
          )

      }
    }
}
