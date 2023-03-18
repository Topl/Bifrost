package co.topl.genusLibrary.interpreter

import cats.data.{Chain, EitherT}
import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.brambl.models.Identifier
import co.topl.brambl.models.transaction._
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.BlockId
import co.topl.genusLibrary.algebras.BlockFetcherAlgebra
import co.topl.genusLibrary.failure.{Failure, Failures}
import co.topl.genusLibrary.model.{BlockData, HeightData}
import co.topl.node.models.BlockBody
import fs2.Stream
import scala.collection.immutable.ListSet

object NodeBlockFetcher {

  def make[F[_]: Async](toplRpc: ToplRpc[F, Stream[F, *]]): Resource[F, BlockFetcherAlgebra[F]] =
    Resource.pure {
      new BlockFetcherAlgebra[F] {
        override def fetch(height: Long): F[Either[Failure, HeightData]] =
          toplRpc
            .blockIdAtHeight(height)
            .flatMap {
              case Some(blockId) =>
                EitherT(fetchBlock[F](blockId, toplRpc))
                  .map(blockData => HeightData(height = height, blockData = blockData.some))
                  .value
              case None =>
                HeightData(
                  height = height,
                  blockData = Option.empty[BlockData]
                ).asRight[Failure].pure[F]
            }
      }
    }

  // TODO: TSDK-186 | Do calls concurrently.
  private def fetchBlock[F[_]: Async](
    blockId: BlockId,
    toplRpc: ToplRpc[F, Stream[F, *]]
  ): F[Either[Failure, BlockData]] = (
    for {
      header       <- EitherT(fetchBlockHeader(blockId, toplRpc))
      body         <- EitherT(fetchBlockBody(blockId, toplRpc))
      transactions <- EitherT(fetchTransactions(body, toplRpc))
    } yield BlockData(header, body, transactions)
  ).value

  private def fetchBlockHeader[F[_]: Async](
    blockId: BlockId,
    toplRpc: ToplRpc[F, Stream[F, *]]
  ): F[Either[Failure, BlockHeader]] =
    toplRpc
      .fetchBlockHeader(blockId)
      .map(_.toRight[Failure](Failures.NoBlockHeaderFoundOnNodeFailure(blockId)))

  private def fetchBlockBody[F[_]: Async](
    blockId: BlockId,
    toplRpc: ToplRpc[F, Stream[F, *]]
  ): F[Either[Failure, BlockBody]] =
    toplRpc
      .fetchBlockBody(blockId)
      .map(_.toRight[Failure](Failures.NoBlockBodyFoundOnNodeFailure(blockId)))

  /*
   * If all transactions were retrieved correctly, then all transactions are returned.
   * If one or more transactions is missing, then a failure listing all missing transactions is returned.
   */
  private def fetchTransactions[F[_]: Async](
    body:    BlockBody,
    toplRpc: ToplRpc[F, Stream[F, *]]
  ): F[Either[Failure, Chain[IoTransaction]]] =
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
    } map [Either[Failure, Chain[IoTransaction]]] (_.left.map(
      Failures.NonExistentTransactionsFailure
    ))

}
