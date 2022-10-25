package co.topl.transactiongenerator.interpreters

import cats.data.Chain
import cats.effect._
import cats.effect.std.{Queue, Random}
import cats.implicits._
import cats.Applicative
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Sized
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.transactiongenerator.algebras.TransactionGenerator
import co.topl.transactiongenerator.models.Wallet
import co.topl.typeclasses.implicits._
import fs2._

object Fs2TransactionGenerator {

  /**
   * Interprets TransactionGenerator using a given `Wallet` and FS2.  Emits a never-ending stream of Transactions,
   * updating the local wallet along the way.
   * @param wallet An initial wallet containing an initial set of spendable UTxOs
   */
  def make[F[_]: Async: Random](wallet: Wallet): F[TransactionGenerator[F, Stream[F, *]]] =
    Sync[F].delay(
      new TransactionGenerator[F, Stream[F, *]] {

        private val parallelism = Runtime.getRuntime.availableProcessors()

        def generateTransactions: F[Stream[F, Transaction]] =
          Queue
            // Create a queue of wallets to process.  The queue is "recursive" in that processing a wallet
            // will subsequently enqueue at least one new wallet after processing.
            .unbounded[F, Wallet]
            .flatTap(_.offer(wallet))
            .map { queue =>
              Stream
                .fromQueueUnterminated(queue)
                .parEvalMapUnordered(parallelism)(nextTransactionOf[F])
                .evalMap { case (transaction, wallet) =>
                  // Now that we've processed the old wallet, determine if the new wallet is big
                  // enough to split in half.
                  Sync[F]
                    .delay(
                      if (wallet.spendableBoxes.size > 10) WalletSplitter.split(wallet, 2)
                      else Vector(wallet)
                    )
                    // Enqueue the updated wallet(s)
                    .flatTap(_.traverse(queue.offer))
                    // And return the transaction
                    .as(transaction)
                }
            }
      }
    )

  /**
   * Given a _current_ wallet, produce a new Transaction and new Wallet.  The generated transaction
   * will spend a random input from the wallet and produce two new outputs
   */
  private def nextTransactionOf[F[_]: Async: Random](wallet: Wallet): F[(Transaction, Wallet)] =
    for {
      (inputBoxId, inputBox) <- pickInput[F](wallet)
      inputs = Chain(Transaction.Unproven.Input(inputBoxId, wallet.propositions(inputBox.evidence), inputBox.value))
      outputs   <- createOutputs[F](inputBox)
      timestamp <- Async[F].realTimeInstant
      schedule = Transaction.Schedule(timestamp.toEpochMilli, 0, Long.MaxValue)
      data <- createData[F]
      unprovenTransaction: Transaction.Unproven = Transaction.Unproven(
        inputs,
        outputs,
        schedule,
        Some(data)
      )
      transaction = unprovenTransaction.prove {
        case HeightLockOneProposition => Proofs.Contextual.HeightLock()
        case p                        => throw new MatchError(p)
      }
      updatedWallet = applyTransaction(wallet)(transaction)
    } yield (transaction, updatedWallet)

  /**
   * Selects a spendable box from the wallet
   */
  private def pickInput[F[_]: Applicative](wallet: Wallet): F[(Box.Id, Box)] =
    wallet.spendableBoxes.toList
      .maxBy(_._2.value.asInstanceOf[Box.Values.Poly].quantity.data)
      .pure[F]

  /**
   * Constructs two outputs from the given input box.  The two outputs will split the input box in half.
   */
  private def createOutputs[F[_]: Applicative](inputBox: Box): F[Chain[Transaction.Output]] = {
    val polyBoxValue = inputBox.value.asInstanceOf[Box.Values.Poly]
    if (polyBoxValue.quantity.data > 1) {
      val quantityOutput0 = polyBoxValue.quantity.data / 2
      val output0 = Transaction.Output(
        simpleFullAddress(HeightLockOneSpendingAddress),
        Box.Values.Poly(Sized.maxUnsafe(quantityOutput0)),
        minting = false
      )
      val output1 = Transaction.Output(
        simpleFullAddress(HeightLockOneSpendingAddress),
        Box.Values.Poly(Sized.maxUnsafe(polyBoxValue.quantity.data - quantityOutput0)),
        minting = false
      )
      Chain(output0, output1)
    } else {
      Chain(
        Transaction.Output(
          simpleFullAddress(HeightLockOneSpendingAddress),
          polyBoxValue,
          minting = false
        )
      )
    }
  }
    .pure[F]

  private def createData[F[_]: Applicative]: F[Transaction.DataTetra] =
    Bytes(Array.fill(100)(3: Byte)).pure[F]


}
