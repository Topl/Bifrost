package co.topl.transactiongenerator.interpreters

import cats.data.OptionT
import cats.effect.Async
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.models.{Box, Transaction}
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
  private def transactionStream[F[_]: Async](toplRpc: ToplRpc[F]) =
    for {
      headersStream <- blockStream(toplRpc)
      stream = headersStream.flatMap(header =>
        Stream
          .eval(
            OptionT(toplRpc.fetchBlockBody(header.id))
              .getOrRaise(new IllegalStateException)
              .map(_.toList)
              .flatMap(
                _.traverse(transactionId =>
                  OptionT(toplRpc.fetchTransaction(transactionId)).getOrRaise(new IllegalStateException)
                )
              )
          )
          .flatMap(Stream.iterable)
      )
    } yield stream

  /**
   * Start from the big-bang block, and emit a stream of forward-traversing block headers
   */
  private def blockStream[F[_]: Async](toplRpc: ToplRpc[F]) =
    for {
      bigBangId <- OptionT(toplRpc.blockIdAtHeight(1)).getOrRaise(new IllegalStateException())
      bigBang   <- OptionT(toplRpc.fetchBlockHeader(bigBangId)).getOrRaise(new IllegalStateException())
      headId    <- OptionT(toplRpc.blockIdAtDepth(0)).getOrRaise(new IllegalStateException())
      head      <- OptionT(toplRpc.fetchBlockHeader(headId)).getOrRaise(new IllegalStateException())
      stream =
        if (bigBangId != headId)
          Stream(bigBang) ++ Stream
            .range(2, head.height)
            .evalMap(height =>
              OptionT(toplRpc.blockIdAtHeight(height))
                .flatMapF(toplRpc.fetchBlockHeader)
                .getOrRaise(new IllegalStateException)
            ) ++ Stream(head)
        else Stream(head)
    } yield stream

  /**
   * Incorporate a Transaction into a Wallet by removing spent outputs and including new outputs.
   */
  private[interpreters] def applyTransaction(wallet: Wallet)(transaction: Transaction): Wallet = {
    val spentBoxIds = transaction.inputs.map(_.boxId).toIterable
    val transactionId = transaction.id.asTypedBytes
    val newBoxes = transaction.outputs.zipWithIndex.collect {
      case (output, index) if wallet.propositions.contains(output.address.spendingAddress.typedEvidence) =>
        val boxId = Box.Id(transactionId, index.toShort)
        val box = Box(output.address.spendingAddress.typedEvidence, output.value)
        (boxId, box)
    }.toIterable
    wallet.copy(spendableBoxIds = wallet.spendableBoxIds -- spentBoxIds ++ newBoxes)
  }

  private[interpreters] val emptyWallet: Wallet =
    Wallet(
      Map.empty,
      Map(
        Fs2TransactionGenerator.HeightLockOneSpendingAddress.typedEvidence ->
        Fs2TransactionGenerator.HeightLockOneProposition
      )
    )
}
