package co.topl.transactiongenerator.interpreters

import cats.Applicative
import cats.data.OptionT
import cats.effect._
import cats.effect.std.Queue
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
  def make[F[_]: Async](
    wallet:        Wallet,
    parallelism:   Int,
    maxWalletSize: Int,
    feeF:          BigInt => BigInt = _ => 0
  ): F[TransactionGenerator[F, Stream[F, *]]] =
    Sync[F].delay(
      new TransactionGenerator[F, Stream[F, *]] {

        def generateTransactions: F[Stream[F, IoTransaction]] =
          Queue
            // Create a queue of wallets to process.  The queue is "recursive" in that processing a wallet
            // will subsequently enqueue at least one new wallet after processing.
            .unbounded[F, Option[Wallet]]
            .flatTap(_.offer(wallet.some))
            .map(queue =>
              Stream
                .fromQueueNoneTerminated(queue)
                .parEvalMapUnordered(parallelism)(nextTransactionOf[F](_, feeF).value)
                .evalMap {
                  case Some((transaction, wallet)) =>
                    // Now that we've processed the old wallet, determine if the new wallet is big
                    // enough to split in half.
                    Sync[F]
                      .delay(
                        if (wallet.spendableBoxes.size > maxWalletSize) WalletSplitter.split(wallet, 2)
                        else Vector(wallet)
                      )
                      // Enqueue the updated wallet(s)
                      .flatTap(_.map(_.some).traverse(queue.offer))
                      // And return the transaction
                      .as(transaction.some)
                  case _ => queue.offer(None).as(none[IoTransaction])
                }
                .collect { case Some(tx) => tx }
            )
      }
    )

  /**
   * Given a _current_ wallet, produce a new Transaction and new Wallet.  The generated transaction
   * will spend a random input from the wallet and produce two new outputs
   */
  private def nextTransactionOf[F[_]: Async](
    wallet: Wallet,
    feeF:   BigInt => BigInt
  ): OptionT[F, (IoTransaction, Wallet)] =
    pickInput[F](wallet).semiflatMap { case (inputBoxId, inputBox) =>
      for {
        predicate <- Attestation.Predicate(inputBox.lock.getPredicate, Nil).pure[F]
        unprovenAttestation = Attestation(Attestation.Value.Predicate(predicate))
        inputs = List(SpentTransactionOutput(inputBoxId, unprovenAttestation, inputBox.value))
        outputs   <- createOutputs[F](inputBox, feeF)
        timestamp <- Async[F].realTimeInstant
        schedule = Schedule(0, Long.MaxValue, timestamp.toEpochMilli)
        datum = Datum.IoTransaction(Event.IoTransaction(schedule, SmallData.defaultInstance))
        unprovenTransaction = IoTransaction.defaultInstance.withInputs(inputs).withOutputs(outputs).withDatum(datum)
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
    }

  /**
   * Selects a spendable box from the wallet
   */
  private def pickInput[F[_]: Applicative](wallet: Wallet): OptionT[F, (TransactionOutputAddress, Box)] =
    OptionT.fromOption[F](wallet.spendableBoxes.toList.maximumByOption(_._2.value.getLvl.quantity: BigInt))

  /**
   * Constructs two outputs from the given input box.  The two outputs will split the input box in half.
   */
  private def createOutputs[F[_]: Applicative](
    inputBox: Box,
    feeF:     BigInt => BigInt
  ): F[List[UnspentTransactionOutput]] = {
    val lvlBoxValue = inputBox.value.getLvl
    val inQuantity: BigInt = lvlBoxValue.quantity
    val spendableQuantity = inQuantity - feeF(inQuantity)
    if (spendableQuantity > 0) {
      val quantityOutput0 = spendableQuantity / 2
      List(quantityOutput0, spendableQuantity - quantityOutput0)
        .filter(_ > 0)
        .map(quantity => UnspentTransactionOutput(HeightLockOneSpendingAddress, Value().withLvl(Value.LVL(quantity))))
    } else {
      Nil
    }
  }
    .pure[F]

}
