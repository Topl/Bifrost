package co.topl.transactiongenerator.interpreters

import cats.Applicative
import cats.data.Chain
import cats.effect._
import cats.effect.std.Queue
import cats.effect.std.Random
import cats.implicits._
import co.topl.brambl.models.Datum
import co.topl.brambl.models.Event
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.box.Box
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.Attestation
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.transaction.Schedule
import co.topl.brambl.models.transaction.SpentTransactionOutput
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.models._
import co.topl.numerics.implicits._
import co.topl.transactiongenerator.algebras.TransactionGenerator
import co.topl.transactiongenerator.models.Wallet
import fs2._
import quivr.models.Proof
import quivr.models.SmallData
import quivr.models.TxBind

object Fs2TransactionGenerator {

  /**
   * Interprets TransactionGenerator using a given `Wallet` and FS2.  Emits a never-ending stream of Transactions,
   * updating the local wallet along the way.
   * @param wallet An initial wallet containing an initial set of spendable UTxOs
   */
  def make[F[_]: Async: Random](
    wallet:        Wallet,
    parallelism:   Int,
    maxWalletSize: Int
  ): F[TransactionGenerator[F, Stream[F, *]]] =
    Sync[F].delay(
      new TransactionGenerator[F, Stream[F, *]] {

        def generateTransactions: F[Stream[F, Transaction]] =
          Queue
            // Create a queue of wallets to process.  The queue is "recursive" in that processing a wallet
            // will subsequently enqueue at least one new wallet after processing.
            .unbounded[F, Wallet]
            .flatTap(_.offer(wallet))
            .map { queue =>
              Stream
                .fromQueueUnterminated(queue)
                .parEvalMapUnordered(parallelism)(nextTransactionOf[F](_))
                .evalMap { case (transaction, wallet) =>
                  // Now that we've processed the old wallet, determine if the new wallet is big
                  // enough to split in half.
                  Sync[F]
                    .delay(
                      if (wallet.spendableBoxes.size > maxWalletSize) WalletSplitter.split(wallet, 2)
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
  private def nextTransactionOf[F[_]: Async: Random](
    wallet: Wallet
  ): F[(Transaction, Wallet)] =
    for {
      (inputBoxId, inputBox) <- pickInput[F](wallet)
      attestation = Attestation().withPredicate(
        Attestation.Predicate(
          wallet.propositions(lockAddressOf(inputBox.lock).getLock32.evidence).getPredicate,
          List(Proof().withHeightRange(Proof.HeightRange(TxBind()))) // TODO TxBind
        )
      )
      inputs = List(SpentTransactionOutput(inputBoxId, attestation, inputBox.value))
      outputs   <- createOutputs[F](inputBox)
      timestamp <- Async[F].realTimeInstant
      schedule = Schedule(0, Long.MaxValue, timestamp.toEpochMilli)
      transaction = IoTransaction(
        inputs,
        outputs,
        Datum.IoTransaction(Event.IoTransaction(schedule, SmallData.defaultInstance))
      )
      updatedWallet = applyTransaction(wallet)(transaction)
    } yield (transaction, updatedWallet)

  /**
   * Selects a spendable box from the wallet
   */
  private def pickInput[F[_]: Applicative](wallet: Wallet): F[(TransactionOutputAddress, Box)] =
    wallet.spendableBoxes.toList
      .maxBy(_._2.value.getLvl.quantity: BigInt)
      .pure[F]

  /**
   * Constructs two outputs from the given input box.  The two outputs will split the input box in half.
   */
  private def createOutputs[F[_]: Applicative](inputBox: Box): F[List[UnspentTransactionOutput]] = {
    val lvlBoxValue = inputBox.value.getLvl
    if (lvlBoxValue.quantity > BigInt(1)) {
      val quantityOutput0 = lvlBoxValue.quantity / 2
      val output0 = UnspentTransactionOutput(
        HeightLockOneSpendingAddress,
        Value().withLvl(Value.LVL(quantityOutput0))
      )
      val output1 = UnspentTransactionOutput(
        HeightLockOneSpendingAddress,
        Value().withLvl(Value.LVL(lvlBoxValue.quantity - quantityOutput0))
      )
      Chain(output0, output1)
    } else {
      Chain(
        UnspentTransactionOutput(
          HeightLockOneSpendingAddress,
          Value().withLvl(lvlBoxValue),
          Datum.UnspentOutput.defaultInstance
        )
      )
    }
  }
    .pure[F]

}
