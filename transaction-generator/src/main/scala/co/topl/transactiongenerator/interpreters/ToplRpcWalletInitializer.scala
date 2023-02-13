package co.topl.transactiongenerator.interpreters

import cats.data.OptionT
import cats.effect.Async
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.proto.models.Transaction
import co.topl.{models => legacyModels}
import co.topl.models.utility._
import legacyModels.TypedIdentifier
import co.topl.transactiongenerator.algebras.WalletInitializer
import co.topl.transactiongenerator.models.Wallet
import fs2._

object ToplRpcWalletInitializer {

  /**
   * "Recovers" a wallet using the given ToplRpc.  The recovery mechanism streams all blocks and transactions, and captures
   * applicable UTxOs in the wallet.
   */
  def make[F[_]: Async](
    toplRpc:                     ToplRpc[F, Stream[F, *]],
    fetchHeaderParallelism:      Int,
    fetchBodyParallelism:        Int,
    fetchTransactionParallelism: Int
  ): F[WalletInitializer[F]] =
    Async[F].delay {
      new WalletInitializer[F] {
        def initialize: F[Wallet] =
          for {
            transactionStream <- transactionStream(
              toplRpc,
              fetchHeaderParallelism,
              fetchBodyParallelism,
              fetchTransactionParallelism
            )
            wallet <- transactionStream.scan(emptyWallet)(applyTransaction(_)(_)).compile.last
          } yield wallet.get
      }
    }

  /**
   * Emits a stream of transactions, starting from the big-bang block and moving forward
   */
  private def transactionStream[F[_]: Async](
    toplRpc:                     ToplRpc[F, Stream[F, *]],
    fetchHeaderParallelism:      Int,
    fetchBodyParallelism:        Int,
    fetchTransactionParallelism: Int
  ): F[Stream[F, Transaction]] =
    for {
      blockIds <- blockIdStream(toplRpc, fetchHeaderParallelism)
      stream = blockIds
        .parEvalMap(fetchBodyParallelism)(blockId =>
          OptionT(toplRpc.fetchBlockBody(blockId))
            .getOrRaise(new IllegalStateException("Block body not found"))
            .map(_.transactionIds)
        )
        .flatMap(Stream.iterable)
        .parEvalMap(fetchTransactionParallelism)(transactionId =>
          OptionT(toplRpc.fetchTransaction(transactionId))
            .getOrRaise(new IllegalStateException("Transaction not found"))
        )
    } yield stream

  /**
   * Start from the big-bang block, and emit a stream of forward-traversing block IDs
   */
  private def blockIdStream[F[_]: Async, S[_]](
    toplRpc:                ToplRpc[F, S],
    fetchHeaderParallelism: Int
  ): F[Stream[F, TypedIdentifier]] =
    for {
      bigBangId <- OptionT(toplRpc.blockIdAtHeight(1))
        .getOrRaise(new IllegalStateException("Unknown Big Bang ID"))
      headId <- OptionT(toplRpc.blockIdAtDepth(0)).getOrRaise(new IllegalStateException("Unknown Canonical Head ID"))
      head <- OptionT(toplRpc.fetchBlockHeader(headId))
        .getOrRaise(new IllegalStateException("Unknown Canonical Head Block"))
      stream =
        if (bigBangId != headId)
          Stream(bigBangId) ++
          Stream
            .range[F, Long](2, head.height)
            .parEvalMap(fetchHeaderParallelism)(height =>
              OptionT(toplRpc.blockIdAtHeight(height))
                .getOrRaise(new IllegalStateException("Block not found at height"))
            ) ++
          Stream(headId)
        else Stream(headId)
    } yield stream

}
