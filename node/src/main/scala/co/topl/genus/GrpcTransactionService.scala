package co.topl.genus

import cats.data.EitherT
import cats.effect.kernel.Async
import cats.implicits._
import cats.implicits.catsSyntaxApplicativeId
import co.topl.genus.services._
import co.topl.genusLibrary.algebras.TransactionFetcherAlgebra
import co.topl.genusLibrary.model.GEs
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

  override def getTransactionByAddressStream(
    request: QueryByAddressRequest,
    ctx:     Metadata
  ): Stream[F, TransactionResponse] =
    Stream.raiseError[F](GEs.UnImplemented).adaptErrorsToGrpc

  override def getTxosByAddress(request: QueryByAddressRequest, ctx: Metadata): F[TxoAddressResponse] =
    Async[F].raiseError[TxoAddressResponse](GEs.UnImplemented).adaptErrorsToGrpc

  override def getTxosByAddressStream(request: QueryByAddressRequest, ctx: Metadata): Stream[F, TxoAddressResponse] =
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
