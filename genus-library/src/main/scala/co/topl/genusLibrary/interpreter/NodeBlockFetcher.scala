package co.topl.genusLibrary.interpreter

import cats.data.{Chain, EitherT}
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.genusLibrary.algebras.BlockFetcherAlgebra
import co.topl.genusLibrary.failure.{Failure, Failures}
import co.topl.genusLibrary.model.{BlockData, HeightData}
import co.topl.models.{BlockBody, Transaction, TypedIdentifier}
import co.topl.consensus.models.{BlockHeader => ConsensusBlockHeader} // TODO remove rename, after remove models
import com.typesafe.scalalogging.LazyLogging

import scala.collection.immutable.ListSet

class NodeBlockFetcher[F[_]: Async](toplRpc: ToplRpc[F, Any]) extends BlockFetcherAlgebra[F] with LazyLogging {

  override def fetch(height: Long): F[Either[Failure, HeightData]] =
    toplRpc
      .blockIdAtHeight(height)
      .flatMap {
        case Some(blockId) =>
          EitherT(fetch(blockId))
            .map(blockData => HeightData(height = height, blockData = blockData.some))
            .value
        case None =>
          HeightData(
            height = height,
            blockData = Option.empty[BlockData]
          ).asRight[Failure].pure[F]
      }

  // TODO: TSDK-186 | Do calls concurrently.
  def fetch(blockId: TypedIdentifier): F[Either[Failure, BlockData]] = (
    for {
      header       <- EitherT(fetchBlockHeader(blockId))
      body         <- EitherT(fetchBlockBody(blockId))
      transactions <- EitherT(fetchTransactions(body))
    } yield BlockData(header, body, transactions)
  ).value

  private def fetchBlockHeader(blockId: TypedIdentifier): F[Either[Failure, ConsensusBlockHeader]] =
    toplRpc
      .fetchBlockHeader(blockId)
      .map(_.toRight[Failure](Failures.NoBlockHeaderFoundOnNodeFailure(blockId)))

  private def fetchBlockBody(blockId: TypedIdentifier): F[Either[Failure, BlockBody]] =
    toplRpc
      .fetchBlockBody(blockId)
      .map(_.toRight[Failure](Failures.NoBlockBodyFoundOnNodeFailure(blockId)))

  /*
   * If all transactions were retrieved correctly, then all transactions are returned.
   * If one or more transactions is missing, then a failure listing all missing transactions is returned.
   */
  private def fetchTransactions(body: BlockBody): F[Either[Failure, Chain[Transaction]]] =
    body.toList.traverse(typedIdentifier =>
      toplRpc
        .fetchTransaction(typedIdentifier)
        .map(maybeTransaction => (typedIdentifier, maybeTransaction))
    ) map { e =>
      e.foldLeft(Chain.empty[Transaction].asRight[ListSet[TypedIdentifier]]) {
        case (Right(transactions), (_, Some(transaction)))     => (transactions :+ transaction).asRight
        case (Right(_), (typedIdentifier, None))               => ListSet(typedIdentifier).asLeft
        case (nonExistentTransactions @ Left(_), (_, Some(_))) => nonExistentTransactions
        case (Left(nonExistentTransactions), (typedIdentifier, None)) =>
          Left(nonExistentTransactions + typedIdentifier)
      }
    } map [Either[Failure, Chain[Transaction]]] (_.left.map(Failures.NonExistentTransactionsFailure))

}
