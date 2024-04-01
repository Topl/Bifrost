package co.topl.blockchain.interpreters

import cats.data.OptionT
import cats.effect.{IO, Sync}
import cats.implicits._
import co.topl.algebras.testInterpreters.TestStore
import co.topl.brambl.common.ContainsImmutable
import co.topl.brambl.models.box.{Attestation, Value}
import co.topl.brambl.models.{Datum, LockAddress, LockId, TransactionId, TransactionOutputAddress}
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.syntax._
import co.topl.codecs.bytes.tetra.TetraScodecCodecs
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.interpreters.{ConsensusDataEventSourcedState, EpochBoundariesEventSourcedState}
import co.topl.consensus.models._
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.interpreters.SchedulerClock
import co.topl.ledger.algebras.TransactionRewardCalculatorAlgebra
import co.topl.ledger.models.RewardQuantities
import co.topl.node.models.{BlockBody, FullBlock, FullBlockBody}
import co.topl.numerics.implicits._
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalamock.munit.AsyncMockFactory
import quivr.models.Int128
import co.topl.models._
import co.topl.models.utility._
import co.topl.proto.node.EpochData

import java.time.Instant
import scala.concurrent.duration._

class EpochDataInterpreterSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  private val lvlValue: Value =
    Value.defaultInstance.withLvl(Value.LVL(Int128(ByteString.copyFrom(BigInt(100).toByteArray))))

  private val emptyLockAddress =
    LockAddress(id = LockId(zeroBytes(32)))

  test("apply and unapply blocks") {
    withMock {
      val tx1 =
        IoTransaction(datum = Datum.IoTransaction.defaultInstance)
          .withOutputs(
            List(
              UnspentTransactionOutput(value = lvlValue, address = emptyLockAddress)
            )
          )
      val List(tx2, tx3, tx4, tx5, tx6, tx7) =
        LazyList
          .unfold(tx1)(parent =>
            IoTransaction(datum = Datum.IoTransaction.defaultInstance)
              .withInputs(List(stxo(parent, 0)))
              .withOutputs(
                List(
                  UnspentTransactionOutput(value = lvlValue, address = emptyLockAddress)
                )
              )
              .some
              .map(v => (v, v))
          )
          .take(6)
          .toList
      val genesisBlock = createGenesisBlock(List(tx1))
      val block2 = createChildBlock(genesisBlock.header, 5)(List(tx2, tx3))
      val block3 = createChildBlock(block2.header, 15)(List(tx4))
      val block4 = createChildBlock(block3.header, 25)(List(tx5, tx6))
      val block5 = createChildBlock(block4.header, 35)(List(tx7))
      val testResource =
        for {
          parentChildTree <- ParentChildTree.FromRef.make[F, BlockId].toResource
          _               <- parentChildTree.associate(genesisBlock.header.id, BlockId(zeroBytes(32))).toResource
          _               <- parentChildTree.associate(block2.header.id, genesisBlock.header.id).toResource
          _               <- parentChildTree.associate(block3.header.id, block2.header.id).toResource
          _               <- parentChildTree.associate(block4.header.id, block3.header.id).toResource
          _               <- parentChildTree.associate(block5.header.id, block4.header.id).toResource
          state           <- TestStore.make[F, Epoch, EpochData].toResource
          clock <- SchedulerClock
            .make[F](1000.milli, 10L, 1L, Instant.ofEpochMilli(genesisBlock.header.timestamp), 0L, () => 0L.pure[F])
          headerStore      <- TestStore.make[F, BlockId, BlockHeader].toResource
          bodyStore        <- TestStore.make[F, BlockId, BlockBody].toResource
          transactionStore <- TestStore.make[F, TransactionId, IoTransaction].toResource
          _ <- List(genesisBlock, block2, block3, block4, block5)
            .traverse(block =>
              headerStore.put(block.header.id, block.header) *>
              bodyStore.put(
                block.header.id,
                BlockBody(block.fullBody.transactions.map(_.id), block.fullBody.rewardTransaction.map(_.id))
              ) *>
              block.fullBody.allTransactions.traverse(tx => transactionStore.put(tx.id, tx))
            )
            .toResource
          rewardCalculator = mock[TransactionRewardCalculatorAlgebra]
          _ = (rewardCalculator
            .rewardsOf(_: IoTransaction))
            .expects(*)
            .anyNumberOfTimes()
            .returning(RewardQuantities(BigInt(50)))
          epochBoundaryEss = mock[EventSourcedState[F, EpochBoundariesEventSourcedState.EpochBoundaries[
            F
          ], BlockId]]
          _ = (epochBoundaryEss
            .useStateAt(_: BlockId)(_: EpochBoundariesEventSourcedState.EpochBoundaries[F] => F[BlockId]))
            .expects(*, *)
            .anyNumberOfTimes()
            .onCall {
              case (_: BlockId, f: (EpochBoundariesEventSourcedState.EpochBoundaries[F] => F[BlockId]) @unchecked) =>
                TestStore
                  .make[F, Epoch, BlockId]
                  .flatTap(_.put(-1, genesisBlock.header.id))
                  .flatTap(_.put(0, block2.header.id))
                  .flatTap(_.put(1, block3.header.id))
                  .flatTap(_.put(2, block4.header.id))
                  .flatTap(_.put(3, block5.header.id))
                  .flatMap(f)
            }
          consensusDataEss = mock[EventSourcedState[F, ConsensusDataEventSourcedState.ConsensusData[
            F
          ], BlockId]]
          _ = (consensusDataEss
            .useStateAt(_: BlockId)(_: ConsensusDataEventSourcedState.ConsensusData[F] => F[(BigInt, BigInt)]))
            .expects(genesisBlock.header.id, *)
            .repeat(3)
            .onCall {
              case (
                    _: BlockId,
                    f: (ConsensusDataEventSourcedState.ConsensusData[F] => F[(BigInt, BigInt)]) @unchecked
                  ) =>
                (
                  TestStore.make[F, Unit, BigInt].flatTap(_.put((), 40)),
                  TestStore.make[F, Unit, BigInt].flatTap(_.put((), 0))
                )
                  .mapN(ConsensusDataEventSourcedState.ConsensusData(_, _, null))
                  .flatMap(f)
            }
          _ = (consensusDataEss
            .useStateAt(_: BlockId)(_: ConsensusDataEventSourcedState.ConsensusData[F] => F[(BigInt, BigInt)]))
            .expects(block2.header.id, *)
            .once()
            .onCall {
              case (
                    _: BlockId,
                    f: (ConsensusDataEventSourcedState.ConsensusData[F] => F[(BigInt, BigInt)]) @unchecked
                  ) =>
                (
                  TestStore.make[F, Unit, BigInt].flatTap(_.put((), 20)),
                  TestStore.make[F, Unit, BigInt].flatTap(_.put((), 20))
                )
                  .mapN(ConsensusDataEventSourcedState.ConsensusData(_, _, null))
                  .flatMap(f)
            }
          _ = (consensusDataEss
            .useStateAt(_: BlockId)(_: ConsensusDataEventSourcedState.ConsensusData[F] => F[(BigInt, BigInt)]))
            .expects(block3.header.id, *)
            .once()
            .onCall {
              case (
                    _: BlockId,
                    f: (ConsensusDataEventSourcedState.ConsensusData[F] => F[(BigInt, BigInt)]) @unchecked
                  ) =>
                (
                  TestStore.make[F, Unit, BigInt].flatTap(_.put((), 30)),
                  TestStore.make[F, Unit, BigInt].flatTap(_.put((), 10))
                )
                  .mapN(ConsensusDataEventSourcedState.ConsensusData(_, _, null))
                  .flatMap(f)
            }
          ess <- EpochDataEventSourcedState.make[F](
            genesisBlock.header.parentHeaderId.pure[F],
            genesisBlock.header.id,
            parentChildTree,
            _ => IO.unit,
            state.pure[F],
            clock,
            headerStore.getOrRaise,
            bodyStore.getOrRaise,
            transactionStore.getOrRaise,
            rewardCalculator,
            epochBoundaryEss,
            consensusDataEss
          )
          canonicalHeadRef <- IO.ref(block5.header.id).toResource
          underTest        <- EpochDataInterpreter.make[F](Sync[F].defer(canonicalHeadRef.get), ess)
          expectedDataMinus1 =
            EpochData(
              epoch = -1,
              eon = 1,
              era = 0,
              isComplete = true,
              startHeight = 1,
              endHeight = 1,
              startSlot = 0,
              endSlot = 0,
              startTimestamp = 0,
              endTimestamp = 0,
              transactionCount = 1,
              totalTransactionReward = 50,
              activeStake = 40,
              inactiveStake = 0,
              dataBytes = blockSize(genesisBlock)
            )
          expectedData0 =
            EpochData(
              epoch = 0,
              eon = 1,
              era = 0,
              isComplete = true,
              startHeight = 2,
              endHeight = 2,
              startSlot = 1,
              endSlot = 10,
              startTimestamp = 1000,
              endTimestamp = 10999,
              transactionCount = 2,
              totalTransactionReward = 100,
              activeStake = 40,
              inactiveStake = 0,
              dataBytes = blockSize(block2)
            )
          expectedData1 = EpochData(
            epoch = 1,
            eon = 1,
            era = 0,
            isComplete = true,
            startHeight = 3,
            endHeight = 3,
            startSlot = 11,
            endSlot = 20,
            startTimestamp = 11000,
            endTimestamp = 20999,
            transactionCount = 1,
            totalTransactionReward = 50,
            activeStake = 40,
            inactiveStake = 0,
            dataBytes = blockSize(block3)
          )
          expectedData2 = EpochData(
            epoch = 2,
            eon = 1,
            era = 0,
            isComplete = true,
            startHeight = 4,
            endHeight = 4,
            startSlot = 21,
            endSlot = 30,
            startTimestamp = 21000,
            endTimestamp = 30999,
            transactionCount = 2,
            totalTransactionReward = 100,
            activeStake = 20,
            inactiveStake = 20,
            dataBytes = blockSize(block4)
          )
          expectedData3 = EpochData(
            epoch = 3,
            eon = 1,
            era = 0,
            isComplete = false,
            startHeight = 5,
            endHeight = 5,
            startSlot = 31,
            endSlot = 40,
            startTimestamp = 31000,
            endTimestamp = 40999,
            transactionCount = 1,
            totalTransactionReward = 50,
            activeStake = 30,
            inactiveStake = 10,
            dataBytes = blockSize(block5)
          )
          _ <- List(
            -1 -> expectedDataMinus1,
            0  -> expectedData0,
            1  -> expectedData1,
            2  -> expectedData2,
            3  -> expectedData3,
            2  -> expectedData2,
            1  -> expectedData1,
            0  -> expectedData0
          ).traverse { case (i, expectedData) =>
            OptionT(underTest.dataOf(i)).getOrRaise(new NoSuchElementException).assertEquals(expectedData).toResource
          }

          // Verify the "unapply" functionality works
          _ <- canonicalHeadRef.set(genesisBlock.header.id).toResource
          _ <- OptionT(underTest.dataOf(-1))
            .getOrRaise(new NoSuchElementException)
            .assertEquals(expectedDataMinus1)
            .toResource
          _ <- underTest.dataOf(0).assertEquals(None).toResource
        } yield ()
      testResource.use_
    }
  }

  private def createGenesisBlock(transactions: Seq[IoTransaction]) = {
    val header = BlockHeader(
      parentHeaderId = BlockId(zeroBytes(32)),
      parentSlot = -1,
      txRoot = zeroBytes(32),
      bloomFilter = zeroBytes(256),
      timestamp = 0L,
      height = 1,
      slot = 0,
      eligibilityCertificate = eligibilityCertificate,
      operationalCertificate = operationalCertificate,
      metadata = ByteString.EMPTY,
      address = StakingAddress(zeroBytes(32)),
      version = ProtocolVersion(0, 0, 1)
    ).embedId
    FullBlock(header, FullBlockBody(transactions))
  }

  private def createChildBlock(parent: BlockHeader, slot: Slot)(transactions: Seq[IoTransaction]): FullBlock = {
    val header = BlockHeader(
      parentHeaderId = parent.id,
      parentSlot = parent.slot,
      txRoot = ByteString.copyFrom(Array.fill(32)(0: Byte)),
      bloomFilter = ByteString.copyFrom(Array.fill(256)(0: Byte)),
      timestamp = System.currentTimeMillis(),
      height = parent.height + 1,
      slot = slot,
      eligibilityCertificate = eligibilityCertificate,
      operationalCertificate = operationalCertificate,
      metadata = ByteString.EMPTY,
      address = StakingAddress(zeroBytes(32)),
      version = ProtocolVersion(0, 0, 1)
    ).embedId
    FullBlock(header, FullBlockBody(transactions))
  }

  private val eligibilityCertificate: EligibilityCertificate =
    EligibilityCertificate(
      zeroBytes(80),
      zeroBytes(32),
      zeroBytes(32),
      zeroBytes(32)
    )

  private val operationalCertificate: OperationalCertificate =
    OperationalCertificate(
      VerificationKeyKesProduct(zeroBytes(32), 0),
      SignatureKesProduct(
        SignatureKesSum(zeroBytes(32), zeroBytes(64), Vector.empty),
        SignatureKesSum(zeroBytes(32), zeroBytes(64), Vector.empty),
        zeroBytes(32)
      ),
      zeroBytes(32),
      zeroBytes(64)
    )

  private def zeroBytes(length: Int): ByteString =
    ByteString.copyFrom(Array.fill[Byte](length)(0))

  private def stxo(tx: IoTransaction, index: Int) =
    SpentTransactionOutput(
      TransactionOutputAddress(id = tx.id).withIndex(index),
      Attestation().withPredicate(Attestation.Predicate.defaultInstance),
      tx.outputs(index).value
    )

  private def blockSize(block: FullBlock) =
    TetraScodecCodecs.consensusBlockHeaderCodec.encode(block.header).require.length + block.fullBody.allTransactions
      .map(ContainsImmutable.instances.ioTransactionImmutable.immutableBytes(_).value.size())
      .sum

}
