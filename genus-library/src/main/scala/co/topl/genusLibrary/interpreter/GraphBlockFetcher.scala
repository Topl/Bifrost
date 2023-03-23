package co.topl.genusLibrary.interpreter

import cats.data.EitherT
import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.typeclasses.implicits._
import co.topl.consensus.models.{BlockHeader, BlockId}
import co.topl.genus.services.BlockData
import co.topl.genusLibrary.algebras.{BlockFetcherAlgebra, VertexFetcherAlgebra}
import co.topl.genusLibrary.model.{GenusException, GenusExceptions}
import co.topl.genusLibrary.orientDb.schema.SchemaBlockHeader.Field
import co.topl.genusLibrary.orientDb.schema.VertexSchemaInstances.instances._
import co.topl.node.models.BlockBody
import com.tinkerpop.blueprints.impls.orient.OrientGraphNoTx
import scala.jdk.CollectionConverters._
import scala.util.Try
import scodec.bits.ByteVector

object GraphBlockFetcher {

  def make[F[_]: Async](
    orientGraph:   OrientGraphNoTx,
    vertexFetcher: VertexFetcherAlgebra[F]
  ): Resource[F, BlockFetcherAlgebra[F]] =
    Resource.pure {
      new BlockFetcherAlgebra[F] {

        override def fetchHeader(blockId: BlockId): F[Either[GenusException, Option[BlockHeader]]] =
          EitherT(vertexFetcher.fetchHeader(blockId))
            .map(_.map(blockHeaderSchema.decodeVertex))
            .value

        override def fetchBody(blockId: BlockId): F[Either[GenusException, Option[BlockBody]]] =
          (for {
            maybeHeaderVertex <- EitherT(vertexFetcher.fetchHeader(blockId))
            headerVertex <- EitherT.fromOption[F](
              maybeHeaderVertex,
              GenusExceptions.NoCurrentHeaderVertex(ByteVector(blockId.value.toByteArray)): GenusException
            )
            maybeBodyVertex <- EitherT(vertexFetcher.fetchBody(headerVertex))
            bodyVertex <- EitherT.fromOption[F](
              maybeBodyVertex,
              GenusExceptions.NoCurrentBodyVertex(ByteVector(blockId.value.toByteArray)): GenusException
            )
            blockBody <- EitherT.pure[F, GenusException](blockBodySchema.decodeVertex(bodyVertex).some)
          } yield blockBody).value

        override def fetchBlock(blockId: BlockId): F[Either[GenusException, Option[BlockData]]] =
          (for {
            maybeHeaderVertex <- EitherT(vertexFetcher.fetchHeader(blockId))

            headerVertex <- EitherT.fromOption[F](
              maybeHeaderVertex,
              GenusExceptions.Message(show"Vertex Header not Found ${blockId.value.show}"): GenusException
            )

            blockBody <- EitherT(vertexFetcher.fetchBody(headerVertex)).flatMap(
              EitherT
                .fromOption[F](
                  _,
                  GenusExceptions.Message(show"Vertex Body not Found ${blockId.value.show}"): GenusException
                )
                .map(blockBodySchema.decodeVertex)
            )

            iterableIoTransaction <- EitherT(vertexFetcher.fetchTransactions(headerVertex))
              .map(_.map(ioTransactionSchema.decodeVertex))

            res = BlockData.of(
              header = blockHeaderSchema.decodeVertex(headerVertex),
              body = blockBody,
              transactions = iterableIoTransaction.toList
            )

          } yield res.some).value

        override def fetchHeaderByHeight(height: Long): F[Either[GenusException, Option[BlockHeader]]] =
          Async[F].delay(
            Try(
              orientGraph.getVertices(blockHeaderSchema.name, Array(Field.Height), Array(height)).asScala
            ).toEither
              .map(_.headOption.map(blockHeaderSchema.decodeVertex))
              .leftMap[GenusException](tx => GenusExceptions.MessageWithCause("FetchHeaderByHeight", tx))
          )

        def fetchBlockByHeight(height: Long): F[Either[GenusException, Option[BlockData]]] =
          (for {
            maybeHeaderVertex <-
              EitherT(
                Async[F].delay(
                  Try(
                    orientGraph.getVertices(blockHeaderSchema.name, Array(Field.Height), Array(height)).asScala
                  ).toEither
                    .map(_.headOption)
                    .leftMap[GenusException](tx => GenusExceptions.MessageWithCause("FetchHeaderByHeight", tx))
                )
              )
            headerVertex <- EitherT.fromOption[F](
              maybeHeaderVertex,
              GenusExceptions.Message(s"No Header Found at height $height"): GenusException
            )

            blockBody <- EitherT(vertexFetcher.fetchBody(headerVertex)).flatMap(
              EitherT
                .fromOption[F](
                  _,
                  GenusExceptions.Message(
                    s"No Body Found at height $height for header ${headerVertex.getId}"
                  ): GenusException
                )
                .map(blockBodySchema.decodeVertex)
            )

            iterableIoTransaction <- EitherT(vertexFetcher.fetchTransactions(headerVertex))
              .map(_.map(ioTransactionSchema.decodeVertex))

            res = BlockData.of(
              header = blockHeaderSchema.decodeVertex(headerVertex),
              body = blockBody,
              transactions = iterableIoTransaction.toList
            )

          } yield res.some).value

      }
    }
}
