package co.topl.genus

import cats.data.EitherT
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.genus.algebras.TransactionFetcherAlgebra
import co.topl.genus.model.GEs
import co.topl.genus.services._
import co.topl.typeclasses.implicits._
import io.grpc.Metadata
import fs2._

class GrpcTransactionService[F[_]: Async](transactionFetcher: TransactionFetcherAlgebra[F])
    extends TransactionServiceFs2Grpc[F, Metadata] {

  override def getTransactionById(request: GetTransactionByIdRequest, ctx: Metadata): F[TransactionResponse] =
    EitherT(transactionFetcher.fetchTransactionReceipt(request.transactionId))
      .foldF(
        ge => Async[F].raiseError[TransactionReceipt](GEs.Internal(ge)),
        _.map(_.pure[F])
          .getOrElse(
            Async[F]
              .raiseError[TransactionReceipt](GEs.NotFound(s"TransactionId:${request.transactionId.show}"))
          )
      )
      .map(TransactionResponse(_))
      .adaptErrorsToGrpc

  override def getTransactionByLockAddressStream(
    request: QueryByLockAddressRequest,
    ctx:     Metadata
  ): Stream[F, TransactionResponse] =
    Stream.raiseError[F](GEs.UnImplemented).adaptErrorsToGrpc

  override def getTxosByLockAddress(request: QueryByLockAddressRequest, ctx: Metadata): F[TxoLockAddressResponse] =
    EitherT(transactionFetcher.fetchTransactionByLockAddress(request.address, request.state))
      .map(TxoLockAddressResponse(_))
      .rethrowT
      .adaptErrorsToGrpc

  override def getTxosByLockAddressStream(
    request: QueryByLockAddressRequest,
    ctx:     Metadata
  ): Stream[F, TxoLockAddressResponse] =
    Stream.raiseError[F](GEs.UnImplemented).adaptErrorsToGrpc

  override def getTxosByAssetLabel(request: QueryByAssetLabelRequest, ctx: Metadata): Stream[F, TxoResponse] =
    Stream.raiseError[F](GEs.UnImplemented).adaptErrorsToGrpc

  override def createOnChainTransactionIndex(
    request: CreateOnChainTransactionIndexRequest,
    ctx:     Metadata
  ): F[CreateOnChainTransactionIndexResponse] =
    Async[F].raiseError[CreateOnChainTransactionIndexResponse](GEs.UnImplemented).adaptErrorsToGrpc

  override def getExistingTransactionIndexes(
    request: GetExistingTransactionIndexesRequest,
    ctx:     Metadata
  ): F[GetExistingTransactionIndexesResponse] =
    Async[F].raiseError[GetExistingTransactionIndexesResponse](GEs.UnImplemented).adaptErrorsToGrpc

  override def getIndexedTransactions(
    request: GetIndexedTransactionsRequest,
    ctx:     Metadata
  ): Stream[F, TransactionResponse] =
    Stream.raiseError[F](GEs.UnImplemented).adaptErrorsToGrpc

  override def dropIndex(request: DropIndexRequest, ctx: Metadata): F[DropIndexResponse] =
    Async[F].raiseError[DropIndexResponse](GEs.UnImplemented).adaptErrorsToGrpc
}
