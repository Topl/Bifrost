package co.topl.transactiongenerator.interpreters

import cats.data.OptionT
import cats.effect.Async
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.models._
import co.topl.transactiongenerator.algebras.WalletInitializer
import co.topl.transactiongenerator.models.Wallet
import fs2._
import co.topl.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._

object ToplRpcWalletInitializer {

  /**
   * "Recovers" a wallet using the given ToplRpc.  The recovery mechanism streams all blocks and transactions, and captures
   * applicable UTxOs in the wallet.
   */
  def make[F[_]: Async](toplRpc: ToplRpc[F]): F[WalletInitializer[F]] =
    Async[F].delay {
      new WalletInitializer[F] {
        def initialize: F[Wallet] =
          for {
            transactionStream <- transactionStream(toplRpc)
            wallet            <- transactionStream.scan(emptyWallet)(applyTransaction(_)(_)).compile.last
          } yield wallet.get
      }
    }

  /**
   * Emits a stream of transactions, starting from the big-bang block and moving forward
   */
  private def transactionStream[F[_]: Async](toplRpc: ToplRpc[F]): F[Stream[F, Transaction]] =
    for {
      headersStream <- blockStream(toplRpc)
      stream = headersStream.flatMap(header =>
        Stream
          .eval(
            OptionT(toplRpc.fetchBlockBody(header.id))
              .getOrRaise(new IllegalStateException("Block body not found"))
              .map(_.toList)
              .flatMap(
                _.traverse(transactionId =>
                  OptionT(toplRpc.fetchTransaction(transactionId))
                    .getOrRaise(new IllegalStateException("Transaction not found"))
                )
              )
          )
          .flatMap(Stream.iterable)
      )
    } yield stream

  /**
   * Start from the big-bang block, and emit a stream of forward-traversing block headers
   */
  private def blockStream[F[_]: Async](toplRpc: ToplRpc[F]): F[Stream[F, BlockHeaderV2]] =
    for {
      bigBangId <- OptionT(toplRpc.blockIdAtHeight(1))
        .getOrRaise(new IllegalStateException("Unknown Big Bang ID"))
      bigBang <- OptionT(toplRpc.fetchBlockHeader(bigBangId))
        .getOrRaise(new IllegalStateException("Unknown Big Bang Block"))
      headId <- OptionT(toplRpc.blockIdAtDepth(0)).getOrRaise(new IllegalStateException("Unknown Canonical Head ID"))
      head <- OptionT(toplRpc.fetchBlockHeader(headId))
        .getOrRaise(new IllegalStateException("Unknown Canonical Head Block"))
      stream =
        if (bigBangId != headId)
          Stream(bigBang) ++
          Stream
            .range(2, head.height)
            .evalMap(height =>
              OptionT(toplRpc.blockIdAtHeight(height))
                .flatMapF(toplRpc.fetchBlockHeader)
                .getOrRaise(new IllegalStateException("Block not found at height"))
            ) ++
          Stream(head)
        else Stream(head)
    } yield stream

}
