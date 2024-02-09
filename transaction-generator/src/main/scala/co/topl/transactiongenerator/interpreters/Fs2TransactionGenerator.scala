package co.topl.transactiongenerator.interpreters

import cats.{Applicative, Monad}
import cats.data.OptionT
import cats.effect._
import cats.effect.std.Random
import cats.implicits._
import co.topl.brambl.models.Datum
import co.topl.brambl.models.Event
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.box._
import co.topl.brambl.common.ContainsSignable._
import co.topl.brambl.common.ContainsSignable.instances._
import co.topl.brambl.models.transaction._
import co.topl.brambl.syntax._
import co.topl.brambl.validation.algebras.TransactionCostCalculator
import co.topl.quivr.api.Prover
import co.topl.transactiongenerator.algebras.TransactionGenerator
import co.topl.transactiongenerator.models.Wallet
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import fs2._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import quivr.models.SmallData

object Fs2TransactionGenerator {

  /**
   * Interprets TransactionGenerator using a given `Wallet` and FS2.  Emits a never-ending stream of Transactions,
   * updating the local wallet along the way.
   * @param wallet An initial wallet containing an initial set of spendable UTxOs
   */
  def make[F[_]: Async: Random](
    wallet:         Wallet,
    costCalculator: TransactionCostCalculator[F],
    metadataF:      F[SmallData]
  ): F[TransactionGenerator[F, Stream[F, *]]] =
    Sync[F].delay(
      new TransactionGenerator[F, Stream[F, *]] {

        implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F]("TransactionGenerator")

        def generateTransactions: F[Stream[F, IoTransaction]] =
          Stream.unfoldEval(wallet)(nextTransactionOf[F](_, costCalculator, metadataF).value).pure[F]
      }
    )

  /**
   * Given a _current_ wallet, produce a new Transaction and new Wallet.  If the wallet is small, create extra UTxOs.
   * If the wallet is large, consolidate UTxOs.
   */
  private def nextTransactionOf[F[_]: Async: Random: Logger](
    wallet:         Wallet,
    costCalculator: TransactionCostCalculator[F],
    metadataF:      F[SmallData]
  ): OptionT[F, (IoTransaction, Wallet)] =
    (if (wallet.spendableBoxes.size < 5) generateExpandingTransaction(wallet, costCalculator, metadataF)
     else generateConsolidatingTransaction(wallet, costCalculator, metadataF))
      .map(transaction => transaction -> applyTransaction(wallet)(transaction))

  /**
   * Constructs a Transaction which attempts to split a UTxO into two
   */
  private def generateExpandingTransaction[F[_]: Async: Random: Logger](
    wallet:         Wallet,
    costCalculator: TransactionCostCalculator[F],
    metadataF:      F[SmallData]
  ): OptionT[F, IoTransaction] =
    pickSingleInput[F](wallet).semiflatMap { case (inputBoxId, inputBox) =>
      for {
        predicate <- Attestation.Predicate(inputBox.lock.getPredicate, Nil).pure[F]
        unprovenAttestation = Attestation(Attestation.Value.Predicate(predicate))
        inputs = List(SpentTransactionOutput(inputBoxId, unprovenAttestation, inputBox.value))
        outputs           <- createManyOutputs[F](inputBox)
        provenTransaction <- formTransaction(costCalculator, metadataF)(inputs, outputs)
      } yield provenTransaction
    }

  /**
   * Constructs a Transaction which attempts to consolidate several UTxOs into one
   */
  private def generateConsolidatingTransaction[F[_]: Async: Logger](
    wallet:         Wallet,
    costCalculator: TransactionCostCalculator[F],
    metadataF:      F[SmallData]
  ): OptionT[F, IoTransaction] =
    OptionT
      .pure[F](
        wallet.spendableBoxes.filter(_._2.value.value.isLvl).toList.sortBy(_._2.value.getLvl.quantity: BigInt).take(15)
      )
      .filter(_.nonEmpty)
      .map(_.map { case (inputBoxId, inputBox) =>
        SpentTransactionOutput(
          inputBoxId,
          Attestation(Attestation.Value.Predicate(Attestation.Predicate(inputBox.lock.getPredicate, Nil))),
          inputBox.value
        )
      })
      .semiflatMap(inputs =>
        formTransaction(costCalculator, metadataF)(
          inputs,
          List(
            UnspentTransactionOutput(
              HeightLockOneSpendingAddress,
              Value.defaultInstance.withLvl(
                Value.LVL(
                  inputs.foldMap(_.value.getLvl.quantity: BigInt)
                )
              )
            )
          )
        )
      )

  /**
   * Constructs a proven Transaction from the given inputs and outputs
   */
  private def formTransaction[F[_]: Async: Logger](
    costCalculator: TransactionCostCalculator[F],
    metadataF:      F[SmallData]
  )(inputs: Seq[SpentTransactionOutput], outputs: Seq[UnspentTransactionOutput]) =
    for {
      timestamp <- Async[F].realTimeInstant
      schedule = Schedule(0, Long.MaxValue, timestamp.toEpochMilli)
      metadata <- metadataF
      datum = Datum.IoTransaction(Event.IoTransaction(schedule, metadata))
      unprovenTransaction <- applyFee(costCalculator)(
        IoTransaction.defaultInstance.withInputs(inputs).withOutputs(outputs).withDatum(datum)
      )
      _     <- Logger[F].info(show"Spending ${unprovenTransaction.inputs.mkString_(", ")}")
      proof <- Prover.heightProver[F].prove((), unprovenTransaction.signable)
      provenTransaction = unprovenTransaction
        .copy(
          inputs = unprovenTransaction.inputs.map(i =>
            i.copy(attestation =
              Attestation(
                Attestation.Value.Predicate(
                  i.attestation.getPredicate.copy(responses = List(proof))
                )
              )
            )
          )
        )
        .embedId
    } yield provenTransaction

  /**
   * Selects a single spendable box from the wallet
   */
  private def pickSingleInput[F[_]: Applicative](wallet: Wallet): OptionT[F, (TransactionOutputAddress, Box)] =
    OptionT.fromOption[F](
      wallet.spendableBoxes.filter(_._2.value.value.isLvl).toList.maximumByOption(_._2.value.getLvl.quantity: BigInt)
    )

  /**
   * Constructs two outputs from the given input box.  The two outputs will split the input box in half.
   */
  private def createManyOutputs[F[_]: Monad: Random](
    inputBox: Box
  ): F[List[UnspentTransactionOutput]] = for {
    lvlBoxValue <- inputBox.value.getLvl.pure[F]
    inQuantity = lvlBoxValue.quantity: BigInt
    spendableQuantity = inQuantity
    outputQuantities <-
      if (spendableQuantity <= 0) List.empty[BigInt].pure[F]
      else if (spendableQuantity == BigInt(1)) List(BigInt(1)).pure[F]
      else if (spendableQuantity == BigInt(2)) List.fill(2)(BigInt(1)).pure[F]
      else {
        Random[F]
          .nextLongBounded(spendableQuantity.toLong / 2)
          .map(spendableQuantity - _)
          .map(quantityOutput0 => List(quantityOutput0, spendableQuantity - quantityOutput0))
      }
    result = outputQuantities
      .filter(_ > 0)
      .map(quantity =>
        UnspentTransactionOutput(HeightLockOneSpendingAddress, Value.defaultInstance.withLvl(Value.LVL(quantity)))
      )
  } yield result

  private def applyFee[F[_]: Monad](
    costCalculator: TransactionCostCalculator[F]
  )(transaction: IoTransaction): F[IoTransaction] =
    for {
      cost <- costCalculator.costOf(transaction)
      updated = transaction.withOutputs(
        transaction.outputs
          .foldLeft((cost, List.empty[UnspentTransactionOutput])) {
            case ((remainingCost, outputs), output) if remainingCost > 0 =>
              output.value.value.lvl.fold((remainingCost, outputs)) { lvl =>
                if (lvl.quantity > remainingCost)
                  (0L -> (outputs :+ output
                    .copy(value = Value.defaultInstance.withLvl(lvl.copy(quantity = lvl.quantity - cost)))))
                else
                  ((remainingCost - (lvl.quantity: BigInt).toLong): Long, outputs)
              }
            case ((_, outputs), output) => (0L, (outputs :+ output))
          }
          ._2
      )
    } yield updated

  def emptyMetadata[F[_]: Applicative]: F[SmallData] =
    SmallData.defaultInstance.pure[F]

  def randomMetadata[F[_]: Monad: Random]: F[SmallData] =
    Random[F].nextBytes(64).map(ByteString.copyFrom).map(SmallData(_))

}
