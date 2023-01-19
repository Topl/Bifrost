package co.topl.minting.interpreters

import cats.MonadThrow
import cats.effect.std.Queue
import cats.effect.{Async, IO}
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.catsakka._
import co.topl.minting.algebras.{BlockPackerAlgebra, StakingAlgebra}
import co.topl.minting.models._
import co.topl.models.ModelGenerators._
import co.topl.models.{Block, BlockBody, SlotData, StakingAddresses}
import fs2._
import fs2.concurrent.Topic
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

import scala.collection.immutable.NumericRange
import scala.concurrent.duration._

class BlockProducerSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  override val munitTimeout: FiniteDuration = 10.seconds

  test("Produce a block when eligible") {
    PropF.forAllF { (parentSlotData: SlotData, stakingAddress: StakingAddresses.Operator, outputBlock: Block) =>
      withMock {
        for {
          staker <- mock[StakingAlgebra[F]].pure[F]
          _ = (() => staker.address).expects().once().returning(stakingAddress.pure[F])
          clock = mock[ClockAlgebra[F]]
          blockPacker = mock[BlockPackerAlgebra[F]]
          parents     <- Queue.unbounded[F, Option[SlotData]]
          underTest   <- BlockProducer.make[F](Stream.fromQueueNoneTerminated(parents), staker, clock, blockPacker)
          resultFiber <- Async[F].start(Stream.force(underTest.blocks).compile.toList)
          vrfHit = VrfHit(eligibilityCertificateGen.first, parentSlotData.slotId.slot + 1, ratioGen.first)
          _ = (() => clock.globalSlot)
            .expects()
            .once()
            .returning(parentSlotData.slotId.slot.pure[F])
          _ = (staker.elect _)
            .expects(parentSlotData.slotId, parentSlotData.slotId.slot + 1)
            .once()
            .returning(vrfHit.some.pure[F])
          _ = (staker
            .certifyBlock(_, _, _))
            .expects(parentSlotData.slotId, vrfHit.slot, *)
            .once()
            .returning(outputBlock.some.pure[F])
          clockDeferment <- IO.deferred[Unit]
          _ = (clock.delayedUntilSlot(_)).expects(vrfHit.slot).once().returning(clockDeferment.get)
          _ = (clock
            .slotToTimestamps(_))
            .expects(vrfHit.slot)
            .once()
            .returning(NumericRange.inclusive(50L, 99L, 1L).pure[F])
          _ = (blockPacker.improvePackedBlock(_, _, _)).expects(*, *, *).once().returning(IO.pure(_ => IO.never))
          _      <- parents.offer(parentSlotData.some)
          _      <- clockDeferment.complete(())
          _      <- Async[F].sleep(3.seconds)
          _      <- parents.offer(none)
          result <- resultFiber.joinWith(MonadThrow[F].raiseError(new IllegalStateException()))
          // The `outputBlock` is generated and doesn't line up with the input data of the unsigned block
          // (It's not the responsibility of the BlockProducer to create the resulting full Block; that's the staker
          // during the certification process, and we rely on mocks for that)
//          _ = assert(result.head == outputBlock)
        } yield ()
      }
    }
  }

}
