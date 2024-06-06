package co.topl.minting.interpreters

import cats.effect.std.Queue
import cats.effect.{Async, IO}
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models.box.{FungibilityType, QuantityDescriptorType}
import co.topl.brambl.models.{GroupId, LockAddress, SeriesId}
import co.topl.brambl.syntax._
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData, StakingAddress}
import co.topl.ledger.algebras.TransactionRewardCalculatorAlgebra
import co.topl.ledger.models.{AssetId, RewardQuantities}
import co.topl.minting.algebras.{BlockPackerAlgebra, StakingAlgebra}
import co.topl.minting.models._
import co.topl.models.ModelGenerators._
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.models.generators.node.ModelGenerators._
import co.topl.node.models.{FullBlock, FullBlockBody}
import com.google.protobuf.ByteString
import fs2._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

import scala.collection.immutable.NumericRange
import scala.concurrent.duration._
import co.topl.algebras.Stats.Implicits._

class BlockProducerSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  override val munitIOTimeout: FiniteDuration = 10.seconds

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMaxSize(3)
      .withMinSuccessfulTests(5)

  test("Produce a block when eligible") {
    PropF.forAllF {
      (
        parentSlotData: SlotData,
        stakingAddress: StakingAddress,
        rewardAddress:  LockAddress,
        outputHeader:   BlockHeader,
        outputBody:     FullBlockBody
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
            .certifyBlock(_, _, _, _))
            .expects(parentSlotData.slotId, vrfHit.slot, *, *)
            .once()
            .returning(outputHeader.some.pure[F])

          if (outputBody.transactions.nonEmpty)
            (() => staker.rewardAddress)
              .expects()
              .once()
              .returning(rewardAddress.pure[F])

          val clock = mock[ClockAlgebra[F]]
          (() => clock.slotsPerEpoch).expects().anyNumberOfTimes().returning(300L.pure[F])
          (() => clock.globalSlot).expects().anyNumberOfTimes().returning((parentSlotData.slotId.slot + 1).pure[F])
          (clock
            .slotToTimestamps(_))
            .expects(vrfHit.slot)
            .once()
            .returning(NumericRange.inclusive(50L, 99L, 1L).pure[F])
          (() => clock.currentTimestamp)
            .expects()
            .once()
            .returning(55L.pure[F])

          val assetId1 = AssetId(
            groupId = GroupId(ByteString.copyFrom(Array.fill[Byte](32)(1))).some,
            seriesId = SeriesId(ByteString.copyFrom(Array.fill[Byte](32)(2))).some,
            groupAlloy = none,
            seriesAlloy = none,
            fungibilityType = FungibilityType.GROUP_AND_SERIES,
            quantityDescriptor = QuantityDescriptorType.LIQUID
          )
          val rewardQuantities = RewardQuantities(BigInt(10L), BigInt(5L), Map(assetId1 -> BigInt(30L)))

          val rewardCalculator = mock[TransactionRewardCalculatorAlgebra]
          (rewardCalculator.rewardsOf(_)).expects(*).anyNumberOfTimes().returning(rewardQuantities)

          for {
            clockDeferment   <- IO.deferred[Unit]
            blockPackerQueue <- Queue.unbounded[F, FullBlockBody]
            _                <- blockPackerQueue.offer(outputBody)
            blockPacker = new BlockPackerAlgebra[F] {

              def blockImprover(parentBlockId: BlockId, height: Long, slot: Long): Stream[F, FullBlockBody] =
                Stream
                  .fromQueueUnterminated(blockPackerQueue)
                  .evalTap(_ => (IO.sleep(100.milli) >> clockDeferment.complete(())).start)
            }
            parents <- Queue.unbounded[F, Option[SlotData]]
            results <- Queue.unbounded[F, Option[FullBlock]]
            underTest <- BlockProducer.make[F](
              Stream.fromQueueNoneTerminated(parents),
              staker,
              clock,
              blockPacker,
              rewardCalculator,
              ().pure[F]
            )
            resultFiber <- Async[F].start(Stream.force(underTest.blocks).enqueueNoneTerminated(results).compile.drain)
            _ = (clock.delayedUntilSlot(_)).expects(vrfHit.slot).once().returning(clockDeferment.get)
            _ <- parents.offer(parentSlotData.some)
            // The `outputBlock` is generated and doesn't line up with the input data of the unsigned block
            // (It's not the responsibility of the BlockProducer to create the resulting full Block; that's the staker
            // during the certification process, and we rely on mocks for that)
            result <- results.take
            _ = assert(result.isDefined)
            _ = assert(result.get.header == outputHeader)
            _ = assert(result.get.fullBody.transactions == outputBody.transactions)
            _ = if (outputBody.transactions.nonEmpty) {
              val rewardTx = result.get.fullBody.rewardTransaction.get
              assert(rewardTx.inputs.length == 1)
              assert(rewardTx.inputs.head.address.id.value == parentSlotData.slotId.blockId.value)
              assert(rewardTx.outputs.length == 3)
              assert(
                rewardTx.outputs.exists(
                  _.value.value.lvl.map(_.quantity: BigInt).contains(BigInt(outputBody.transactions.length * 10L))
                )
              )
              assert(
                rewardTx.outputs.exists(
                  _.value.value.topl.map(_.quantity: BigInt).contains(BigInt(outputBody.transactions.length * 5L))
                )
              )
              assert(
                rewardTx.outputs.exists(
                  _.value.value.asset.exists(a =>
                    (a.quantity: BigInt) == BigInt(outputBody.transactions.length * 30L)
                    &&
                    a.groupId == assetId1.groupId &&
                    a.seriesId == assetId1.seriesId &&
                    a.groupAlloy == assetId1.groupAlloy &&
                    a.seriesAlloy == assetId1.seriesAlloy &&
                    a.fungibility == assetId1.fungibilityType &&
                    a.quantityDescriptor == assetId1.quantityDescriptor
                  )
                )
              )
            }
            _ <- parents.offer(none)
            _ <- results.take.assertEquals(None)
            _ <- resultFiber.joinWithNever
          } yield ()
        }
    }
  }

}
