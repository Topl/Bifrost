package co.topl.genusLibrary.interpreter

import cats.data.EitherT
import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.brambl.models.{LockAddress, TransactionId}
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.genus.services.{ChainDistance, ConfidenceFactor, TransactionReceipt, Txo, TxoState}
import co.topl.genusLibrary.algebras.{TransactionFetcherAlgebra, VertexFetcherAlgebra}
import co.topl.genusLibrary.model.GE
import co.topl.genusLibrary.orientDb.instances.SchemaBlockHeader
import co.topl.genusLibrary.orientDb.instances.VertexSchemaInstances.instances._
import co.topl.genusLibrary.orientDb.schema.EdgeSchemaInstances.addressTxoEdge
import com.tinkerpop.blueprints.{Direction, Vertex}
import scala.jdk.CollectionConverters._
import scala.util.Try

object GraphTransactionFetcher {

  def make[F[_]: Async](vertexFetcher: VertexFetcherAlgebra[F]): Resource[F, TransactionFetcherAlgebra[F]] =
    Resource.pure {
      new TransactionFetcherAlgebra[F] {

        override def fetchTransaction(
          transactionId: TransactionId
        ): F[Either[GE, Option[IoTransaction]]] =
          EitherT(vertexFetcher.fetchTransaction(transactionId))
            .map(_.map(ioTransactionSchema.decodeVertex))
            .value

        override def fetchTransactionReceipt(
          transactionId: TransactionId
        ): F[Either[GE, Option[TransactionReceipt]]] = {
          val res = (for {
            ioTxVertex <- EitherT(vertexFetcher.fetchTransaction(transactionId))
            blockHeaderVertex <- EitherT.fromEither[F](
              ioTxVertex
                .flatMap(v => Try(v.getProperty[Vertex](SchemaBlockHeader.Field.BlockId)).toOption)
                .asRight[GE]
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
                  depth = ChainDistance(blockHeader.height)
                ).some
              case _ => None
            }

          }

        }

        override def fetchTransactionByLockAddress(lockAddress: LockAddress, state: TxoState): F[Either[GE, Seq[Txo]]] =
          (for {
            lockAddressVertex <- EitherT(vertexFetcher.fetchLockAddress(lockAddress))
            txos <- EitherT.fromEither[F](
              lockAddressVertex
                .map(_.getVertices(Direction.OUT, addressTxoEdge.label).asScala.map(txoSchema.decodeVertex).toSeq)
                .map(_.filter(_.state.value == state.value))
                .getOrElse(Seq.empty)
                .asRight[GE]
            )
          } yield txos).value
      }

    }
}
