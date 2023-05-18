package co.topl.blockchain

import cats.effect.IO
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.blockchain.PrivateTestnet.DefaultTotalStake
import co.topl.brambl.models.box.Value
import co.topl.models.NetworkPrefix
import co.topl.models.utility.Ratio
import co.topl.numerics.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory

class StakerInitializersSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("Big bang outputs for PrivateTestnet, currentTime, 1 staker") {
    for {
      implicit0(networkPrefix: NetworkPrefix) <- NetworkPrefix(1: Byte).pure[F]
      timestamp                               <- System.currentTimeMillis().pure[F]
      operator                                <- PrivateTestnet.stakerInitializers(timestamp, 1).pure[F]
      bigBangOutputs <- Async[F].delay(operator.head.bigBangOutputs(Ratio(DefaultTotalStake, 1: BigInt).round))

      _ <- assertIO(bigBangOutputs.size.pure[F], 2)
      _ <- assertIOBoolean(bigBangOutputs.forall(_.address == operator.head.lockAddress).pure[F])
      _ <- assertIOBoolean(
        bigBangOutputs
          .map(_.value)
          .contains(
            Value().withTopl(Value.TOPL(Ratio(DefaultTotalStake, 1: BigInt).round, operator.head.stakingAddress.some))
          )
          .pure[F]
      )
      _ <- assertIOBoolean(
        bigBangOutputs
          .map(_.value)
          .contains(
            Value().withRegistration(Value.Registration(operator.head.registration, operator.head.stakingAddress))
          )
          .pure[F]
      )
    } yield ()
  }

// TODO ask some suggestion about which kind of unit testing should we create
//  test("Big bang outputs for PrivateTestnet, currentTime, n staker")
//  test("Big bang outputs for other Testnet, currentTime, 1 staker")
}
