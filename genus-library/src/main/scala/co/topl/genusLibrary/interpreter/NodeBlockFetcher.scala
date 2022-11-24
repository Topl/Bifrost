package co.topl.genusLibrary.interpreter

import cats.data.{Chain, EitherT}
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.genusLibrary.algebras.{BlockFetcherAlgebra, SequenceResponse, ServiceResponse}
import co.topl.genusLibrary.failure.{Failure, Failures}
import co.topl.models.{BlockBodyV2, BlockHeaderV2, BlockV2, Transaction, TypedIdentifier}
import fs2.Stream

import scala.collection.immutable.ListSet

class NodeBlockFetcher[F[_]: Async](toplRpc: ToplRpc[F, Any]) extends BlockFetcherAlgebra[F, Stream[F, *]] {

  override def fetchHeaders(height: Long): SequenceResponse[F, Stream[F, *], Option[BlockHeaderV2]] = Async[F].delay {
    Stream
      // Range from given height to "positive infinity". If height is one, then the range would be [1, 2, 3, ...]
      .range(height, Long.MaxValue)
      .covary[F]
      // Map each height to a block id. Range ends up being [blockId_01, blockId02, blockId03, ...]
      .evalMap(toplRpc.blockIdAtHeight)
      .evalMap {
        case Some(blockId) =>
          val blockHeader: F[Either[Failure, BlockHeaderV2]] = fetchBlockHeader(blockId)
          blockHeader.map(_.map(_.some))
        case None => Option.empty[BlockHeaderV2].asRight[Failure].pure[F]
      }
  }

  // TODO: TSDK-186 | Do calls concurrently.
  override def fetch(height: Long): ServiceResponse[F, Option[BlockV2.Full]] = toplRpc.blockIdAtHeight(height) flatMap {
    case None => Option.empty[BlockV2.Full].asRight[Failure].pure[F]
    case Some(blockId) =>
      (
        for {
          header       <- EitherT(fetchBlockHeader(blockId))
          body         <- EitherT(fetchBlockBody(blockId))
          transactions <- EitherT(fetchTransactions(body))
        } yield Option(BlockV2.Full(header, transactions))
      ).value
  }

  private def fetchBlockHeader(blockId: TypedIdentifier): ServiceResponse[F, BlockHeaderV2] =
    toplRpc
      .fetchBlockHeader(blockId)
      .map(_.toRight[Failure](Failures.NoBlockHeaderFoundOnNodeFailure(blockId)))

  private def fetchBlockBody(blockId: TypedIdentifier): ServiceResponse[F, BlockBodyV2] =
    toplRpc
      .fetchBlockBody(blockId)
      .map(_.toRight[Failure](Failures.NoBlockBodyFoundOnNodeFailure(blockId)))

  /*
   * If all transactions were retrieved correctly, then all transactions are returned.
   * If one or more transactions is missing, then a failure listing all missing transactions is returned.
   */
  private def fetchTransactions(body: BlockBodyV2): ServiceResponse[F, Chain[Transaction]] =
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
