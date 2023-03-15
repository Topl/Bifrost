package co.topl.transactiongenerator.interpreters

import cats.Applicative
import cats.effect._
import cats.effect.std.Queue
import cats.effect.std.Random
import cats.implicits._
import co.topl.brambl.models.Datum
import co.topl.brambl.models.Event
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.box._
import co.topl.brambl.common.ContainsSignable._
import co.topl.brambl.common.ContainsSignable.instances._
import co.topl.brambl.models.transaction._
import co.topl.numerics.implicits._
import co.topl.quivr.api.Prover
import co.topl.transactiongenerator.algebras.TransactionGenerator
import co.topl.transactiongenerator.models.Wallet
import fs2._
import quivr.models.SmallData

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

        def generateTransactions: F[Stream[F, IoTransaction]] =
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
  private def nextTransactionOf[F[_]: Async](
    wallet: Wallet
  ): F[(IoTransaction, Wallet)] =
    for {
      (inputBoxId, inputBox) <- pickInput[F](wallet)
      predicate = Attestation.Predicate(inputBox.lock.getPredicate, Nil)
      unprovenAttestation = Attestation(Attestation.Value.Predicate(predicate))
      inputs = List(SpentTransactionOutput(inputBoxId, unprovenAttestation, inputBox.value))
      outputs   <- createOutputs[F](inputBox)
      timestamp <- Async[F].realTimeInstant
      schedule = Schedule(0, Long.MaxValue, timestamp.toEpochMilli)
      unprovenTransaction = IoTransaction(
        inputs,
        outputs,
        Datum.IoTransaction(Event.IoTransaction(schedule, SmallData.defaultInstance))
      )
      proof <- Prover.heightProver[F].prove((), unprovenTransaction.signable)
      provenTransaction = unprovenTransaction.copy(
        inputs = unprovenTransaction.inputs.map(
          _.copy(attestation =
            Attestation(
              Attestation.Value.Predicate(
                predicate.copy(responses = List(proof))
              )
            )
          )
        )
      )
      updatedWallet = applyTransaction(wallet)(provenTransaction)
    } yield (provenTransaction, updatedWallet)

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
      List(output0, output1)
    } else {
      List(
        UnspentTransactionOutput(
          HeightLockOneSpendingAddress,
          Value().withLvl(lvlBoxValue)
        )
      )
    }
  }
    .pure[F]

}
