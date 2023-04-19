package co.topl.genusLibrary.interpreter

import cats.data.{Chain, EitherT, OptionT}
import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.brambl.models.Identifier
import co.topl.brambl.models.transaction._
import co.topl.consensus.models.BlockId
import co.topl.genus.services.BlockData
import co.topl.genusLibrary.algebras.NodeBlockFetcherAlgebra
import co.topl.genusLibrary.model.{GE, GEs}
import co.topl.node.models.BlockBody
import fs2.Stream
import org.typelevel.log4cats.Logger

import scala.collection.immutable.ListSet

object NodeBlockFetcher {

  def make[F[_]: Async: Logger](
    toplRpc: ToplRpc[F, Stream[F, *]]
  ): Resource[F, NodeBlockFetcherAlgebra[F, Stream[F, *]]] =
    Resource.pure {

      new NodeBlockFetcherAlgebra[F, Stream[F, *]] {
        override def fetch(startHeight: Long, endHeight: Long): F[Stream[F, BlockData]] =
          Async[F].delay {
            Stream
              // Range from given start height to either defined max height or "positive infinity".
              // If start height is one, then the range would be [1, 2, 3, ...]
              .range(startHeight, endHeight)
              .covary[F]
              .evalMap(fetch)
              .evalMapFilter {
                case Left(ex) =>
                  Logger[F]
                    .error(s"Unexpected error while fetching block. Error=[$ex]")
                    .as(Option.empty[BlockData])
                case Right(None) =>
                  Logger[F]
                    .info(s"No block found.")
                    .as(Option.empty[BlockData])
                case Right(blockData @ Some(_)) =>
                  (blockData: Option[BlockData]).pure[F]
              }
          }

        override def fetch(height: Long): F[Either[GE, Option[BlockData]]] =
          toplRpc
            .blockIdAtHeight(height)
            .flatMap {
              case Some(blockId) =>
                EitherT(fetchBlock(blockId, toplRpc)).map(_.some).value
              case None =>
                Option.empty[BlockData].asRight[GE].pure[F]
            }

        // TODO: TSDK-186 | Do calls concurrently.
        private def fetchBlock(blockId: BlockId, toplRpc: ToplRpc[F, Stream[F, *]]): F[Either[GE, BlockData]] = (
          for {
            header       <- OptionT(toplRpc.fetchBlockHeader(blockId)).toRight(GEs.HeaderNotFound(blockId): GE)
            body         <- OptionT(toplRpc.fetchBlockBody(blockId)).toRight(GEs.BodyNotFound(blockId): GE)
            transactions <- EitherT(fetchTransactions(body, toplRpc))
          } yield BlockData(header, body, transactions.toList)
        ).value

        /*
         * If all transactions were retrieved correctly, then all transactions are returned.
         * If one or more transactions is missing, then a GenusException listing all missing transactions is returned.
         */
        private def fetchTransactions(
          body:    BlockBody,
          toplRpc: ToplRpc[F, Stream[F, *]]
        ): F[Either[GE, Chain[IoTransaction]]] =
          body.transactionIds.toList.traverse(ioTx32 =>
            toplRpc
              .fetchTransaction(ioTx32)
              .map(maybeTransaction => (ioTx32, maybeTransaction))
          ) map { e =>
            e.foldLeft(Chain.empty[IoTransaction].asRight[ListSet[Identifier.IoTransaction32]]) {
              case (Right(transactions), (_, Some(transaction)))     => (transactions :+ transaction).asRight
              case (Right(_), (ioTx32, None))                        => ListSet(ioTx32).asLeft
              case (nonExistentTransactions @ Left(_), (_, Some(_))) => nonExistentTransactions
              case (Left(nonExistentTransactions), (ioTx32, None)) =>
                Left(nonExistentTransactions + ioTx32)
            }
          } map [Either[GE, Chain[IoTransaction]]] (_.left.map(
            GEs.TransactionsNotFound
          ))

        def fetchHeight(): F[Option[Long]] =
          (for {
            headBlockId <- OptionT(toplRpc.blockIdAtDepth(depth = 0))
            blockHeader <- OptionT(toplRpc.fetchBlockHeader(headBlockId))
          } yield blockHeader.height).value

      }
    }
}
