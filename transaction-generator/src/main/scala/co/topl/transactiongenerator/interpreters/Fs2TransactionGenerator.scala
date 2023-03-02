package co.topl.transactiongenerator.interpreters

import cats.data.{Chain, EitherT}
import cats.effect._
import cats.effect.std.{Queue, Random}
import cats.implicits._
import cats.Applicative
import co.topl.brambl.models.Datum
import co.topl.brambl.models.Event
import co.topl.brambl.models.box.Box
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.Schedule
import co.topl.brambl.models.transaction.SpentTransactionOutput
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility._
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
  def make[F[_]: Async: Random](
    wallet:                Wallet,
    parallelism:           Int,
    maxWalletSize:         Int,
    transactionDataLength: Int
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
                .parEvalMapUnordered(parallelism)(nextTransactionOf[F](_, transactionDataLength))
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
    wallet:                Wallet,
    transactionDataLength: Int
  ): F[(Transaction, Wallet)] =
    for {
      (inputBoxId, inputBox) <- pickInput[F](wallet)
      inputs = Chain(SpentTransactionOutput(inputBoxId, wallet.propositions(inputBox.evidence), inputBox.value))
      outputs   <- createOutputs[F](inputBox)
      timestamp <- Async[F].realTimeInstant
      schedule = Schedule(0, Long.MaxValue, timestamp.toEpochMilli)
      data <- createData[F](transactionDataLength: Int)
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
      // TODO Wallet spendingAddress model should change to new protobuf specs and not use Isomorphism
      protoTransaction <-
        EitherT(co.topl.models.utility.transactionIsomorphism[F].abMorphism.aToB(transaction.pure[F]))
          .getOrRaise(new RuntimeException("transactionIsomorphism"))
      updatedWallet = applyTransaction(wallet)(protoTransaction)
    } yield (transaction, updatedWallet)

  /**
   * Selects a spendable box from the wallet
   */
  private def pickInput[F[_]: Applicative](wallet: Wallet): F[(BoxId, Box)] =
    wallet.spendableBoxes.toList
      .maxBy(_._2.value.asInstanceOf[Box.Values.Poly].quantity.data)
      .pure[F]

  /**
   * Constructs two outputs from the given input box.  The two outputs will split the input box in half.
   */
  private def createOutputs[F[_]: Applicative](inputBox: Box): F[Chain[UnspentTransactionOutput]] = {
    val polyBoxValue = inputBox.value.getLvl
    if (polyBoxValue.quantity > BigInt(1)) {
      val quantityOutput0 = polyBoxValue.quantity / 2
      val output0 = UnspentTransactionOutput(
        simpleFullAddress(HeightLockOneSpendingAddress),
        Value().withLvl(Value.LVL(quantityOutput0))
      )
      val output1 = UnspentTransactionOutput(
        simpleFullAddress(HeightLockOneSpendingAddress),
        Value().withLvl(Value.LVL(polyBoxValue.quantity - quantityOutput0))
      )
      Chain(output0, output1)
    } else {
      Chain(
        UnspentTransactionOutput(
          simpleFullAddress(HeightLockOneSpendingAddress),
          polyBoxValue,
          Datum.UnspentOutput.defaultInstance
        )
      )
    }
  }
    .pure[F]

}
