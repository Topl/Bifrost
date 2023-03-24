package co.topl.genusLibrary.interpreter

import cats.data.EitherT
import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.{BlockHeader, BlockId}
import co.topl.genus.services.BlockData
import co.topl.genusLibrary.algebras.{BlockFetcherAlgebra, VertexFetcherAlgebra}
import co.topl.genusLibrary.model.{GenusException, GenusExceptions}
import co.topl.genusLibrary.orientDb.schema.VertexSchemaInstances.instances._
import co.topl.node.models.BlockBody
import com.tinkerpop.blueprints.Vertex
import scodec.bits.ByteVector

object GraphBlockFetcher {

  def make[F[_]: Async](vertexFetcher: VertexFetcherAlgebra[F]): Resource[F, BlockFetcherAlgebra[F]] =
    Resource.pure {
      new BlockFetcherAlgebra[F] {

        override def fetchHeader(blockId: BlockId): F[Either[GenusException, Option[BlockHeader]]] =
          EitherT(vertexFetcher.fetchHeader(blockId))
            .map(_.map(blockHeaderSchema.decodeVertex))
            .value

        override def fetchHeaderByHeight(height: Long): F[Either[GenusException, Option[BlockHeader]]] =
          EitherT(vertexFetcher.fetchHeaderByHeight(height))
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

        /**
         * Auxiliary def used by fetch BlockById and BlockByHeight
         * @param f a function that returns a Header Vertex
         * @return
         */
        private def fetchBlockFromVertex(
          f: () => F[Either[GenusException, Option[Vertex]]]
        ): F[Either[GenusException, Option[BlockData]]] =
          EitherT(f())
            .flatMap {
              case Some(headerVertex) =>
                val header = blockHeaderSchema.decodeVertex(headerVertex)

                EitherT(vertexFetcher.fetchBody(headerVertex))
                  .flatMap {
                    case Some(bodyVertex) =>
                      val body = blockBodySchema.decodeVertex(bodyVertex)
                      EitherT.pure[F, GenusException]((header.some, body, Seq.empty[IoTransaction]))
                    case None =>
                      EitherT.pure[F, GenusException](
                        (header.some, BlockBody(Seq.empty), Seq.empty[IoTransaction])
                      )
                  }
                  .flatMap { case (bh, bb, _) =>
                    EitherT(vertexFetcher.fetchTransactions(headerVertex))
                      .map(_.map(ioTransactionSchema.decodeVertex))
                      .map(ioTxs => (bh, bb, ioTxs.toSeq))
                  }

              case None =>
                EitherT.pure[F, GenusException](
                  (Option.empty[BlockHeader], BlockBody(Seq.empty), Seq.empty[IoTransaction])
                )
            }
            .map { case (header, blockBody, ioTransaction) =>
              header.map(header =>
                BlockData.of(
                  header = header,
                  body = blockBody,
                  transactions = ioTransaction
                )
              )
            }
            .value

        override def fetchBlock(blockId: BlockId): F[Either[GenusException, Option[BlockData]]] =
          fetchBlockFromVertex(() => vertexFetcher.fetchHeader(blockId))

        override def fetchBlockByHeight(height: Long): F[Either[GenusException, Option[BlockData]]] =
          fetchBlockFromVertex(() => vertexFetcher.fetchHeaderByHeight(height))

      }
    }
}
