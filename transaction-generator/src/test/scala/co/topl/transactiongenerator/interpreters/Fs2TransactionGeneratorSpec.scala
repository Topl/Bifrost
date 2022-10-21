package co.topl.transactiongenerator.interpreters

import cats.data.Chain
import cats.effect.IO
import cats.implicits._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.{Box, Transaction}
import co.topl.models.utility.Sized
import munit.CatsEffectSuite

class Fs2TransactionGeneratorSpec extends CatsEffectSuite {
  type F[A] = IO[A]

  test("Produces a stream of transactions") {
    for {
      seedTransaction <- Transaction(
        Chain.empty,
        Chain(
          Transaction.Output(
            Fs2TransactionGenerator.simpleFullAddress(Fs2TransactionGenerator.HeightLockOneSpendingAddress),
            Box.Values.Poly(Sized.maxUnsafe(BigInt(1000000))),
            minting = false
          )
        ),
        Transaction.Schedule(0, 0, 0),
        None
      ).pure[F]
      wallet = ToplRpcWalletInitializer.applyTransaction(ToplRpcWalletInitializer.emptyWallet)(seedTransaction)
      underTest <- Fs2TransactionGenerator.make[F](wallet)
      stream    <- underTest.generateTransactions
      result    <- stream.take(50000).compile.toList
      _ = assert(result.length === 50000)
    } yield ()
  }
}
