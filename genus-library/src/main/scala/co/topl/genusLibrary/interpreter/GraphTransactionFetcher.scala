package co.topl.genusLibrary.interpreter

import cats.data.EitherT
import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.brambl.models.Identifier
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.genus.services.{ChainDistance, ConfidenceFactor, TransactionReceipt}
import co.topl.genusLibrary.algebras.{TransactionFetcherAlgebra, VertexFetcherAlgebra}
import co.topl.genusLibrary.model.GE
import co.topl.genusLibrary.orientDb.instances.VertexSchemaInstances.instances._
import com.tinkerpop.blueprints.impls.orient.OrientVertex
import scala.util.Try

object GraphTransactionFetcher {

  def make[F[_]: Async](vertexFetcher: VertexFetcherAlgebra[F]): Resource[F, TransactionFetcherAlgebra[F]] =
    Resource.pure {
      new TransactionFetcherAlgebra[F] {

        override def fetchTransaction(
          ioTransaction32: Identifier.IoTransaction32
        ): F[Either[GE, Option[IoTransaction]]] =
          EitherT(vertexFetcher.fetchTransaction(ioTransaction32))
            .map(_.map(ioTransactionSchema.decodeVertex))
            .value

        override def fetchTransactionReceipt(
          ioTransaction32: Identifier.IoTransaction32
        ): F[Either[GE, Option[TransactionReceipt]]] = {
          val res = (for {
            ioTxVertex <- EitherT(vertexFetcher.fetchTransaction(ioTransaction32))
            blockHeaderVertex <- EitherT.fromEither[F](
              ioTxVertex.flatMap(v => Try(v.getProperty[OrientVertex]("blockId")).toOption).asRight[GE]
            )
          } yield (ioTxVertex, blockHeaderVertex)).value

          res.map {
            _.map {
              case (Some(ioTxV), Some(blockHeaderV)) =>
                val blockHeader = blockHeaderSchema.decodeVertex(blockHeaderV)
                TransactionReceipt(
                  transaction = ioTransactionSchema.decodeVertex(ioTxV),
                  ConfidenceFactor.defaultInstance,
                  blockId = blockHeader.id,
                  depth = ChainDistance(value = blockHeader.height)
                ).some
              case _ => None
            }

          }

        }

      }
    }
}
