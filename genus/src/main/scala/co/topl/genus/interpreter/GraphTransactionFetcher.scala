package co.topl.genus.interpreter

import cats.data.EitherT
import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.{LockAddress, TransactionId}
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.genus.algebras.{TransactionFetcherAlgebra, VertexFetcherAlgebra}
import co.topl.genus.model.{GE, GEs}
import co.topl.genus.orientDb.instances.SchemaBlockHeader
import co.topl.genus.services._
import co.topl.genus.orientDb.instances.VertexSchemaInstances.instances._
import co.topl.genus.orientDb.schema.EdgeSchemaInstances.addressTxoEdge
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
            .map(_.map(ioTransactionSchema.decode))
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
                val blockHeader = blockHeaderSchema.decode(blockHeaderV)
                TransactionReceipt(
                  transaction = ioTransactionSchema.decode(ioTxV),
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
            txos <-
              EitherT.fromEither[F](
                Try(
                  lockAddressVertex.toSeq
                    .flatMap(_.getVertices(Direction.OUT, addressTxoEdge.label).asScala)
                    .map(txoSchema.decode)
                    .filter(_.state.value == state.value)
                ).toEither
                  .leftMap[GE](tx =>
                    GEs.InternalMessageCause("GraphTransactionFetcher:fetchTransactionByLockAddress", tx)
                  )
              )
          } yield txos).value
      }

    }
}
