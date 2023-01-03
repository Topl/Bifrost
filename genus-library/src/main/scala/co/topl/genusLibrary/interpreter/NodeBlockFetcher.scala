package co.topl.genusLibrary.interpreter

import cats.data.{Chain, EitherT}
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.genusLibrary.algebras.{BlockFetcherAlgebra, ServiceResponse}
import co.topl.genusLibrary.failure.{Failure, Failures}
import co.topl.models.{Block, BlockBody, BlockHeader, Transaction, TypedIdentifier}

import scala.collection.immutable.ListSet

class NodeBlockFetcher[F[_]: Async](toplRpc: ToplRpc[F, Any]) extends BlockFetcherAlgebra[F] {

  // TODO: TSDK-186 | Do calls concurrently.
  override def fetch(height: Long): ServiceResponse[F, Option[Block.Full]] = toplRpc.blockIdAtHeight(height) flatMap {
    case None => Option.empty[Block.Full].asRight[Failure].pure[F]
    case Some(blockId) =>
      (
        for {
          header       <- EitherT(fetchBlockHeader(blockId))
          body         <- EitherT(fetchBlockBody(blockId))
          transactions <- EitherT(fetchTransactions(body))
        } yield Option(Block.Full(header, transactions))
      ).value
  }

  private def fetchBlockHeader(blockId: TypedIdentifier): ServiceResponse[F, BlockHeader] =
    toplRpc
      .fetchBlockHeader(blockId)
      .map(_.toRight[Failure](Failures.NoBlockHeaderFoundOnNodeFailure(blockId)))

  private def fetchBlockBody(blockId: TypedIdentifier): ServiceResponse[F, BlockBody] =
    toplRpc
      .fetchBlockBody(blockId)
      .map(_.toRight[Failure](Failures.NoBlockBodyFoundOnNodeFailure(blockId)))

  /*
   * If all transactions were retrieved correctly, then all transactions are returned.
   * If one or more transactions is missing, then a failure listing all missing transactions is returned.
   */
  private def fetchTransactions(body: BlockBody): ServiceResponse[F, Chain[Transaction]] =
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
