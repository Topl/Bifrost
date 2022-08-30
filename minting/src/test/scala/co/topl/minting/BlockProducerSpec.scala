package co.topl.minting

import akka.NotUsed
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.testkit.scaladsl.{TestSink, TestSource}
import cats.effect.{IO, Sync}
import cats.implicits._
import cats.~>
import co.topl.catsakka._
import co.topl.models.ModelGenerators._
import co.topl.models.{BlockBodyV2, BlockV2, SlotData, StakingAddresses}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import akka.actor.typed.scaladsl.adapter._
import akka.stream.scaladsl.Keep
import co.topl.algebras.ClockAlgebra
import co.topl.minting.algebras.LeaderElectionMintingAlgebra.VrfHit
import co.topl.minting.algebras.{BlockPackerAlgebra, StakingAlgebra}

import scala.concurrent.Future

class BlockProducerSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  ResourceFixture(AkkaCatsRuntime.systemResource[F, Nothing](ActorSystem(Behaviors.empty, "BlockProducerSpec")))
    .test("Produce a block when eligible") { implicit system =>
      implicit val _ioToFuture: IO ~> Future = ioToFuture(systemToRuntime(system))
      PropF.forAllF { (parentSlotData: SlotData, stakingAddress: StakingAddresses.Operator, outputBlock: BlockV2) =>
        withMock {
          for {
            (pub, source) <- Sync[F].delay(TestSource.probe[SlotData](system.toClassic).preMaterialize())
            staker = mock[StakingAlgebra[F]]
            _ = (() => staker.address).expects().once().returning(stakingAddress.pure[F])
            clock = mock[ClockAlgebra[F]]
            blockPacker = mock[BlockPackerAlgebra[F]]
            underTest <- BlockProducer.make[F](source.mapMaterializedValue(_ => NotUsed), staker, clock, blockPacker)
            blocks    <- underTest.blocks
            sub       <- blocks.toMat(TestSink[BlockV2])(Keep.right).liftTo[F]
            _ = sub.request(1)
            vrfHit = VrfHit(eligibilityCertificateGen.first, parentSlotData.slotId.slot + 1, ratioGen.first)
            _ = (staker.elect _)
              .expects(parentSlotData, parentSlotData.slotId.slot + 1)
              .once()
              .returning(vrfHit.some.pure[F])
            _ = (staker.certifyBlock(_, _, _))
              .expects(parentSlotData.slotId, vrfHit.slot, *)
              .once()
              .returning(
                outputBlock.some.pure[F]
              )
            clockDeferment <- IO.deferred[Unit]
            _ = (clock.delayedUntilSlot(_)).expects(vrfHit.slot).once().returning(clockDeferment.get)
            _ = (() => clock.currentTimestamp).expects().once().returning(System.currentTimeMillis().pure[F])
            _ = (blockPacker.improvePackedBlock(_)).expects(*).once().returning(IO.pure(_ => IO.never))
            _ = pub.sendNext(parentSlotData)
            _ <- clockDeferment.complete(())
            next = sub.expectNext()
            // The `outputBlock` is generated and doesn't line up with the input data of the unsigned block
            // (It's not the responsibility of the BlockProducer to create the resulting full Block; that's the staker
            // during the certification process, and we rely on mocks for that)
            _ = assert(next == outputBlock)
          } yield ()
        }
      }
    }

  ResourceFixture(AkkaCatsRuntime.systemResource[F, Nothing](ActorSystem(Behaviors.empty, "BlockProducerSpec")))
    .test("Abandon an attempt if a new parent block arrives") { implicit system =>
      implicit val _ioToFuture: IO ~> Future = ioToFuture(systemToRuntime(system))
      PropF.forAllF {
        (
          parentSlotData:  SlotData,
          parentSlotData2: SlotData,
          stakingAddress:  StakingAddresses.Operator,
          outputBlock:     BlockV2
        ) =>
          withMock {
            for {
              (pub, source) <- Sync[F].delay(TestSource.probe[SlotData](system.toClassic).preMaterialize())
              staker = mock[StakingAlgebra[F]]
              _ = (() => staker.address).expects().once().returning(stakingAddress.pure[F])
              clock = mock[ClockAlgebra[F]]
              blockPacker = mock[BlockPackerAlgebra[F]]
              underTest <- BlockProducer.make[F](source.mapMaterializedValue(_ => NotUsed), staker, clock, blockPacker)
              blocks    <- underTest.blocks
              sub       <- blocks.toMat(TestSink[BlockV2])(Keep.right).liftTo[F]
              // Setup the expectations for the first attempt
              // The first attempt comes up with a "distant" eligibility, which we trick the clock into seeming
              // like "forever".
              _ = sub.request(1)
              vrfHit = VrfHit(eligibilityCertificateGen.first, parentSlotData.slotId.slot + 2, ratioGen.first)
              _ = (staker.elect _)
                .expects(parentSlotData, parentSlotData.slotId.slot + 1)
                .once()
                .returning(none[VrfHit].pure[F])
              _ = (staker.elect _)
                .expects(parentSlotData, parentSlotData.slotId.slot + 2)
                .once()
                .returning(vrfHit.some.pure[F])
              // In this particular case, we're using a "clockDeferment" as more of a signal that the clock was requested
              _ = (clock
                .delayedUntilSlot(_)).expects(vrfHit.slot).once().returning(IO.never)
              clockDeferment <- IO.deferred[Unit]
              _ = (blockPacker.improvePackedBlock(_))
                .expects(*)
                .once()
                .returning(IO.pure(_ => IO.defer(clockDeferment.complete(())).void >> IO.never[BlockBodyV2.Full]))

              // Send the first "parent"
              _ = pub.sendNext(parentSlotData)

              // Wait for the clock signal to be received before moving onto the next block
              // (This is only because we're using scalamock which doesn't like the whole "abandonment" flow)
              _ <- clockDeferment.get

              // Now that the first attempt has started, setup the expectations for the second attempt
              // The second attempt is eligibile immediately after the new parent, and we instruct the clock
              // to signal immediately
              vrfHit2 = VrfHit(eligibilityCertificateGen.first, parentSlotData2.slotId.slot + 1, ratioGen.first)
              _ = (staker.elect _)
                .expects(parentSlotData2, parentSlotData2.slotId.slot + 1)
                .once()
                .returning(vrfHit2.some.pure[F])
              _ = (staker.certifyBlock(_, _, _))
                .expects(parentSlotData2.slotId, vrfHit2.slot, *)
                .once()
                .returning(outputBlock.some.pure[F])
              clockDeferment2 <- IO.deferred[Unit]
              _ = (clock.delayedUntilSlot(_)).expects(vrfHit2.slot).once().returning(clockDeferment2.get)
              _ = (() => clock.currentTimestamp).expects().once().returning(System.currentTimeMillis().pure[F])
              _ = (blockPacker.improvePackedBlock(_)).expects(*).once().returning(IO.pure(_ => IO.never))
              _ = pub.sendNext(parentSlotData2)
              _ <- clockDeferment2.complete(())
              next = sub.expectNext()
              _ = assert(next == outputBlock)
            } yield ()
          }
      }
    }

}
