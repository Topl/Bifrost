package co.topl.transactiongenerator.interpreters

import cats.MonadThrow
import cats.data.OptionT
import cats.effect.Async
import cats.implicits._
import co.topl.algebras.NodeRpc
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.BlockId
import co.topl.transactiongenerator.algebras.WalletInitializer
import co.topl.transactiongenerator.models.Wallet
import fs2._

object ToplRpcWalletInitializer {

  /**
   * "Recovers" a wallet using the given ToplRpc.  The recovery mechanism streams all blocks and transactions, and captures
   * applicable UTxOs in the wallet.
   */
  def make[F[_]: Async](
    toplRpc:                     NodeRpc[F, Stream[F, *]],
    fetchBodyParallelism:        Int,
    fetchTransactionParallelism: Int
  ): F[WalletInitializer[F]] =
    Async[F].delay {
      new WalletInitializer[F] {
        def initialize: F[Wallet] =
          for {
            transactionStream <- transactionStream(
              toplRpc,
              fetchBodyParallelism,
              fetchTransactionParallelism
            )
            wallet <- transactionStream.scan(emptyWallet)(applyTransaction(_)(_)).compile.lastOrError
            _      <- MonadThrow[F].raiseWhen(wallet.spendableBoxes.isEmpty)(new IllegalStateException("Empty wallet"))
          } yield wallet
      }
    }

  /**
   * Emits a stream of transactions, starting from the big-bang block and moving forward
   */
  private def transactionStream[F[_]: Async](
    toplRpc:                     NodeRpc[F, Stream[F, *]],
    fetchBodyParallelism:        Int,
    fetchTransactionParallelism: Int
  ): F[Stream[F, IoTransaction]] =
    for {
      blockIds <- blockIdStream(toplRpc)
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
    toplRpc: NodeRpc[F, S]
  ): F[Stream[F, BlockId]] =
    Stream
      .iterate(1L)(_ + 1)
      .evalMap(toplRpc.blockIdAtHeight)
      .unNoneTerminate
      .pure[F]

}
