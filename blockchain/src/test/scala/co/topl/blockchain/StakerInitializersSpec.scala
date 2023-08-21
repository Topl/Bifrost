package co.topl.blockchain

import cats.effect.IO
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.blockchain.PrivateTestnet.DefaultTotalStake
import co.topl.brambl.models.box.Value
import co.topl.models.utility.Ratio
import co.topl.numerics.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory

class StakerInitializersSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("Big bang outputs for PrivateTestnet, currentTime, 1 staker") {
    for {
      timestamp <- System.currentTimeMillis().pure[F]
      operator  <- PrivateTestnet.stakerInitializers(timestamp, 1).pure[F]
      bigBangOutputs <- Async[F].delay(
        operator.head.registrationTransaction(Ratio(DefaultTotalStake, 1: BigInt).round).outputs
      )

      _ <- assertIO(bigBangOutputs.size.pure[F], 1)
      _ <- assertIOBoolean(bigBangOutputs.forall(_.address == operator.head.lockAddress).pure[F])
      _ <- assertIOBoolean(
        bigBangOutputs
          .map(_.value)
          .contains(
            Value.defaultInstance
              .withTopl(Value.TOPL(Ratio(DefaultTotalStake, 1: BigInt).round, operator.head.registration.some))
          )
          .pure[F]
      )
    } yield ()
  }

  test("Big bang outputs for PrivateTestnet, currentTime + 5 seg, 1 staker") {
    for {
      timestamp <- (System.currentTimeMillis() + 5000).pure[F]
      operator  <- PrivateTestnet.stakerInitializers(timestamp, 1).pure[F]
      bigBangOutputs <- Async[F].delay(
        operator.head.registrationTransaction(Ratio(DefaultTotalStake, 1: BigInt).round).outputs
      )

      _ <- assertIO(bigBangOutputs.size.pure[F], 1)
      _ <- assertIOBoolean(bigBangOutputs.forall(_.address == operator.head.lockAddress).pure[F])
      _ <- assertIOBoolean(
        bigBangOutputs
          .map(_.value)
          .contains(
            Value.defaultInstance
              .withTopl(Value.TOPL(Ratio(DefaultTotalStake, 1: BigInt).round, operator.head.registration.some))
          )
          .pure[F]
      )
    } yield ()
  }

  test("Big bang outputs for PrivateTestnet, currentTime, 10 stakers") {
    for {
      timestamp <- System.currentTimeMillis().pure[F]
      operator  <- PrivateTestnet.stakerInitializers(timestamp, 2).pure[F]
      _ <- operator
        .zip(LazyList.from(1))
        .traverse { case (operator, index) =>
          val bigBangOutputs = operator.registrationTransaction(Ratio(DefaultTotalStake, index: BigInt).round).outputs

          assertIO(bigBangOutputs.size.pure[F], 1) &>
          assertIOBoolean(bigBangOutputs.forall(_.address == operator.lockAddress).pure[F]) &>
          assertIOBoolean(
            bigBangOutputs
              .map(_.value)
              .contains(
                Value.defaultInstance
                  .withTopl(Value.TOPL(Ratio(DefaultTotalStake, index: BigInt).round, operator.registration.some))
              )
              .pure[F]
          )

        }
        .void
    } yield ()
  }

}
