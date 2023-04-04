package co.topl.genusLibrary.interpreter

import cats.data.EitherT
import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.brambl.models.box.Box
import co.topl.brambl.models.{Address, Identifier, LockAddress, TransactionOutputAddress}
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.genus.services.{ChainDistance, ConfidenceFactor, TransactionReceipt, Txo, TxoState}
import co.topl.genusLibrary.algebras.{TransactionFetcherAlgebra, VertexFetcherAlgebra}
import co.topl.genusLibrary.model.{GE, GEs}
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

        /**
         * Retrieve TxOs (spent or unspent) that are associated with any of the specifiedaddresses
         *
         * @param addresses sequence of address
         * @return
         */
        override def fetchTransactionsByAddress(addresses: Seq[Address]): F[Either[GE, Map[String, Txo]]] = {
          val box: Box = Box.defaultInstance // TODO Ask how to create a box
          val state: TxoState = TxoState.PENDING // TODO Ask how to create a state
          TransactionOutputAddress.defaultInstance // TODO Ask how to create a box
          LockAddress.defaultInstance // TODO Ask how to create a box
          EitherT
            .fromOption[F](
              Some(Map.from(Seq("foo" -> Txo(box, state, outputAddress = None, lockAddress = None)))),
              GEs.UnImplemented
            )
            .leftWiden[GE]
            .value

        }
      }
    }
}
