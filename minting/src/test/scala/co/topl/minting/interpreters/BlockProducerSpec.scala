package co.topl.minting.interpreters

import cats.effect.std.Queue
import cats.effect.Async
import cats.effect.IO
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.brambl.models.LockAddress
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.SlotData
import co.topl.consensus.models.StakingAddress
import co.topl.ledger.algebras.TransactionRewardCalculatorAlgebra
import co.topl.minting.algebras.BlockPackerAlgebra
import co.topl.minting.algebras.StakingAlgebra
import co.topl.minting.models._
import co.topl.models.ModelGenerators._
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.brambl.generators.ModelGenerators._
import co.topl.node.models.FullBlock
import fs2._
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

import scala.collection.immutable.NumericRange
import scala.concurrent.duration._

class BlockProducerSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  override val munitTimeout: FiniteDuration = 10.seconds

  test("Produce a block when eligible") {
    PropF.forAllF {
      (
        parentSlotData: SlotData,
        stakingAddress: StakingAddress,
        outputHeader:   BlockHeader,
        rewardAddress:  LockAddress
      ) =>
        withMock {
          val vrfHit =
            VrfHit(arbitraryEligibilityCertificate.arbitrary.first, parentSlotData.slotId.slot + 1, ratioGen.first)
          val staker = mock[StakingAlgebra[F]]

          (() => staker.address).expects().once().returning(stakingAddress.pure[F])
          (staker.elect _)
            .expects(parentSlotData.slotId, parentSlotData.slotId.slot + 1)
            .once()
            .returning(vrfHit.some.pure[F])
          (staker
            .certifyBlock(_, _, _))
            .expects(parentSlotData.slotId, vrfHit.slot, *)
            .once()
            .returning(outputHeader.some.pure[F])

          val clock = mock[ClockAlgebra[F]]
          (() => clock.slotsPerEpoch).expects().once().returning(300L.pure[F])
          (() => clock.globalSlot).expects().twice().returning((parentSlotData.slotId.slot + 1).pure[F])
          (clock
            .slotToTimestamps(_))
            .expects(vrfHit.slot)
            .once()
            .returning(NumericRange.inclusive(50L, 99L, 1L).pure[F])

          val blockPacker = mock[BlockPackerAlgebra[F]]
          (blockPacker.improvePackedBlock(_, _, _)).expects(*, *, *).once().returning(IO.pure(_ => IO.never))

          val rewardCalculator = mock[TransactionRewardCalculatorAlgebra[F]]

          for {
            parents <- Queue.unbounded[F, Option[SlotData]]
            results <- Queue.unbounded[F, Option[FullBlock]]
            underTest <- BlockProducer.make[F](
              Stream.fromQueueNoneTerminated(parents),
              staker,
              clock,
              blockPacker,
              rewardCalculator,
              rewardAddress
            )
            resultFiber <- Async[F].start(Stream.force(underTest.blocks).enqueueNoneTerminated(results).compile.drain)
            clockDeferment <- IO.deferred[Unit]
            _ = (clock.delayedUntilSlot(_)).expects(vrfHit.slot).once().returning(clockDeferment.get)
            _ <- parents.offer(parentSlotData.some)
            _ <- clockDeferment.complete(())
            // The `outputBlock` is generated and doesn't line up with the input data of the unsigned block
            // (It's not the responsibility of the BlockProducer to create the resulting full Block; that's the staker
            // during the certification process, and we rely on mocks for that)
            result <- results.take
            _ = assert(result.isDefined)
            _ = assert(result.get.header == outputHeader)
            _ <- parents.offer(none)
            _ <- results.take.assertEquals(None)
            _ <- resultFiber.joinWithNever
          } yield ()
        }
    }
  }

}
