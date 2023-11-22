package co.topl.transactiongenerator.interpreters

import cats.effect.IO
import cats.effect.std.{Random, SecureRandom}
import cats.implicits._
import co.topl.brambl.models.Datum
import co.topl.brambl.models.Event
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.transaction.Schedule
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.brambl.validation.{TransactionCostCalculatorInterpreter, TransactionCostConfig}
import co.topl.numerics.implicits._
import munit.CatsEffectSuite
import quivr.models.SmallData

class Fs2TransactionGeneratorSpec extends CatsEffectSuite {
  type F[A] = IO[A]

  test("Produces a stream of transactions") {
    for {
      seedTransaction <-
        IoTransaction.defaultInstance
          .withOutputs(
            Seq(
              UnspentTransactionOutput(
                address = HeightLockOneSpendingAddress,
                value = Value.defaultInstance.withLvl(Value.LVL(1000000))
              )
            )
          )
          .withDatum(
            Datum.IoTransaction(Event.IoTransaction(Schedule(0, 0, 0), SmallData.defaultInstance))
          )
          .pure[F]
      wallet = applyTransaction(emptyWallet)(seedTransaction)
      implicit0(random: Random[F]) <- SecureRandom.javaSecuritySecureRandom[F]
      costCalculator = TransactionCostCalculatorInterpreter.make[F](TransactionCostConfig())
      underTest <- Fs2TransactionGenerator.make[F](wallet, costCalculator)
      stream    <- underTest.generateTransactions
      _         <- stream.take(500).compile.count.assertEquals(500L)
    } yield ()
  }
}
