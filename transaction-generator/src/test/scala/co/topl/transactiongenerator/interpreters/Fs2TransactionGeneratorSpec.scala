package co.topl.transactiongenerator.interpreters

import cats.effect.IO
import cats.effect.std.Random
import cats.implicits._
import munit.CatsEffectSuite
import com.google.protobuf.ByteString

class Fs2TransactionGeneratorSpec extends CatsEffectSuite {
  type F[A] = IO[A]

  test("Produces a stream of transactions") {
    for {
      seedTransaction <- co.topl.proto.models
        .Transaction(
          Seq.empty,
          Seq(
            co.topl.proto.models.Transaction.UnspentOutput(
              address = simpleFullAddressProto(HeightLockOneSpendingAddressProto).some,
              value = co.topl.proto.models.PolyBoxValue
                .of(co.topl.proto.models.Int128.of(ByteString.copyFrom(BigInt(1000000).toByteArray)).some),
              minting = false
              // metadata = ByteString.EMPTY
            )
          ),
          co.topl.proto.models.Transaction.Schedule.of(0, 0, 0).some
        )
        .pure[F]
      wallet = applyTransaction(emptyWallet)(seedTransaction)
      implicit0(random: Random[F]) <- Random.javaSecuritySecureRandom[F]
      underTest                    <- Fs2TransactionGenerator.make[F](wallet, 1, 10, 100)
      stream                       <- underTest.generateTransactions
      result                       <- stream.take(500).compile.toList
      _ = assert(result.length === 500)
    } yield ()
  }
}
