package co.topl.blockchain.interpreters

import cats.data.OptionT
import cats.effect.{IO, Sync}
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.algebras.testInterpreters.TestStore
import co.topl.brambl.common.ContainsImmutable
import co.topl.brambl.models.box.{Attestation, Value}
import co.topl.brambl.models.{Datum, LockAddress, LockId, TransactionId, TransactionOutputAddress}
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.codecs.bytes.tetra.TetraScodecCodecs
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.interpreters.{ConsensusDataEventSourcedState, EpochBoundariesEventSourcedState}
import co.topl.consensus.models._
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.ledger.algebras.TransactionRewardCalculatorAlgebra
import co.topl.node.models.{BlockBody, FullBlock, FullBlockBody}
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalamock.munit.AsyncMockFactory
import quivr.models.Int128
import co.topl.models._
import co.topl.proto.node.EpochData
import scala.collection.immutable.NumericRange

class EpochDataInterpreterSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  private val lvlValue: Value =
    Value().withLvl(Value.LVL(Int128(ByteString.copyFrom(BigInt(100).toByteArray))))

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
          clock = mock[ClockAlgebra[F]]
          _ = (() => clock.slotsPerEpoch).expects().anyNumberOfTimes().returning(10L.pure[F])
          _ = (clock
            .slotToTimestamps(_: Slot))
            .expects(*)
            .anyNumberOfTimes()
            .onCall((slot: Slot) => NumericRange.inclusive[Long](slot * 1000, (slot + 1) * 1000 - 1, 1).pure[F])
          headerStore      <- TestStore.make[F, BlockId, BlockHeader].toResource
          bodyStore        <- TestStore.make[F, BlockId, BlockBody].toResource
          transactionStore <- TestStore.make[F, TransactionId, IoTransaction].toResource
          _ <- List(genesisBlock, block2, block3, block4, block5)
            .traverse(block =>
              headerStore.put(block.header.id, block.header) *>
              bodyStore.put(block.header.id, BlockBody(block.fullBody.transactions.map(_.id))) *>
              block.fullBody.transactions.traverse(tx => transactionStore.put(tx.id, tx))
            )
            .toResource
          rewardCalculator = mock[TransactionRewardCalculatorAlgebra[F]]
          _ = (rewardCalculator.rewardOf(_: IoTransaction)).expects(*).anyNumberOfTimes().returning(BigInt(50).pure[F])
          epochBoundaryEss = mock[EventSourcedState[F, EpochBoundariesEventSourcedState.EpochBoundaries[
            F
          ], BlockId]]
          _ = (epochBoundaryEss
            .useStateAt(_: BlockId)(_: EpochBoundariesEventSourcedState.EpochBoundaries[F] => F[BlockId]))
            .expects(*, *)
            .twice()
            .onCall { case (_: BlockId, f: (EpochBoundariesEventSourcedState.EpochBoundaries[F] => F[BlockId])) =>
              TestStore
                .make[F, Epoch, BlockId]
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
            .repeat(2)
            .onCall { case (_: BlockId, f: (ConsensusDataEventSourcedState.ConsensusData[F] => F[(BigInt, BigInt)])) =>
              (
                TestStore.make[F, Unit, BigInt].flatTap(_.put((), 40)),
                TestStore.make[F, Unit, BigInt].flatTap(_.put((), 0))
              )
                .mapN(ConsensusDataEventSourcedState.ConsensusData(null, _, _, null))
                .flatMap(f)
            }
          _ = (consensusDataEss
            .useStateAt(_: BlockId)(_: ConsensusDataEventSourcedState.ConsensusData[F] => F[(BigInt, BigInt)]))
            .expects(block2.header.id, *)
            .once()
            .onCall { case (_: BlockId, f: (ConsensusDataEventSourcedState.ConsensusData[F] => F[(BigInt, BigInt)])) =>
              (
                TestStore.make[F, Unit, BigInt].flatTap(_.put((), 20)),
                TestStore.make[F, Unit, BigInt].flatTap(_.put((), 20))
              )
                .mapN(ConsensusDataEventSourcedState.ConsensusData(null, _, _, null))
                .flatMap(f)
            }
          _ = (consensusDataEss
            .useStateAt(_: BlockId)(_: ConsensusDataEventSourcedState.ConsensusData[F] => F[(BigInt, BigInt)]))
            .expects(block3.header.id, *)
            .once()
            .onCall { case (_: BlockId, f: (ConsensusDataEventSourcedState.ConsensusData[F] => F[(BigInt, BigInt)])) =>
              (
                TestStore.make[F, Unit, BigInt].flatTap(_.put((), 30)),
                TestStore.make[F, Unit, BigInt].flatTap(_.put((), 10))
              )
                .mapN(ConsensusDataEventSourcedState.ConsensusData(null, _, _, null))
                .flatMap(f)
            }
          ess <- EpochDataEventSourcedState.make[F](
            BlockId(zeroBytes(32)).pure[F],
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
          expectedData0 =
            EpochData(
              epoch = 0,
              eon = 1,
              era = 0,
              isComplete = true,
              startHeight = 1,
              endHeight = 2,
              startSlot = 0,
              endSlot = 9,
              startTimestamp = 0,
              endTimestamp = 9999,
              transactionCount = 3,
              totalTransactionReward = co.topl.proto.node.Int128.of(ByteString.copyFrom(BigInt(150).toByteArray)),
              activeStake = co.topl.proto.node.Int128.of(ByteString.copyFrom(BigInt(40).toByteArray)),
              inactiveStake = co.topl.proto.node.Int128.of(ByteString.copyFrom(BigInt(0).toByteArray)),
              dataBytes = blockSize(genesisBlock) + blockSize(block2)
            )
          expectedData1 = EpochData(
            epoch = 1,
            eon = 1,
            era = 0,
            isComplete = true,
            startHeight = 3,
            endHeight = 3,
            startSlot = 10,
            endSlot = 19,
            startTimestamp = 10000,
            endTimestamp = 19999,
            transactionCount = 1,
            totalTransactionReward = co.topl.proto.node.Int128.of(ByteString.copyFrom(BigInt(50).toByteArray)),
            activeStake = co.topl.proto.node.Int128.of(ByteString.copyFrom(BigInt(40).toByteArray)),
            inactiveStake = co.topl.proto.node.Int128.of(ByteString.copyFrom(BigInt(0).toByteArray)),
            dataBytes = blockSize(block3)
          )
          expectedData2 = EpochData(
            epoch = 2,
            eon = 1,
            era = 0,
            isComplete = true,
            startHeight = 4,
            endHeight = 4,
            startSlot = 20,
            endSlot = 29,
            startTimestamp = 20000,
            endTimestamp = 29999,
            transactionCount = 2,
            totalTransactionReward = co.topl.proto.node.Int128.of(ByteString.copyFrom(BigInt(100).toByteArray)),
            activeStake = co.topl.proto.node.Int128.of(ByteString.copyFrom(BigInt(20).toByteArray)),
            inactiveStake = co.topl.proto.node.Int128.of(ByteString.copyFrom(BigInt(20).toByteArray)),
            dataBytes = blockSize(block4)
          )
          expectedData3 = EpochData(
            epoch = 3,
            eon = 1,
            era = 0,
            isComplete = false,
            startHeight = 5,
            endHeight = 5,
            startSlot = 30,
            endSlot = 39,
            startTimestamp = 30000,
            endTimestamp = 39999,
            transactionCount = 1,
            totalTransactionReward = co.topl.proto.node.Int128.of(ByteString.copyFrom(BigInt(50).toByteArray)),
            activeStake = co.topl.proto.node.Int128.of(ByteString.copyFrom(BigInt(30).toByteArray)),
            inactiveStake = co.topl.proto.node.Int128.of(ByteString.copyFrom(BigInt(10).toByteArray)),
            dataBytes = blockSize(block5)
          )
          _ <- List(
            0 -> expectedData0,
            1 -> expectedData1,
            2 -> expectedData2,
            3 -> expectedData3,
            2 -> expectedData2,
            1 -> expectedData1,
            0 -> expectedData0
          ).traverse { case (i, expectedData) =>
            OptionT(underTest.dataOf(i)).getOrRaise(new NoSuchElementException).assertEquals(expectedData).toResource
          }

          // Verify the "unapply" functionality works
          _ <- canonicalHeadRef.set(genesisBlock.header.id).toResource
          altExpectedData0 =
            EpochData(
              epoch = 0,
              eon = 1,
              era = 0,
              isComplete = false,
              startHeight = 1,
              endHeight = 1,
              startSlot = 0,
              endSlot = 9,
              startTimestamp = 0,
              endTimestamp = 9999,
              transactionCount = 1,
              totalTransactionReward = co.topl.proto.node.Int128.of(ByteString.copyFrom(BigInt(50).toByteArray)),
              activeStake = co.topl.proto.node.Int128.of(ByteString.copyFrom(BigInt(40).toByteArray)),
              inactiveStake = co.topl.proto.node.Int128.of(ByteString.copyFrom(BigInt(0).toByteArray)),
              dataBytes = blockSize(genesisBlock)
            )
          _ <- OptionT(underTest.dataOf(0))
            .getOrRaise(new NoSuchElementException)
            .assertEquals(altExpectedData0)
            .toResource
          _ <- underTest.dataOf(1).assertEquals(None).toResource
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
      timestamp = System.currentTimeMillis(),
      height = 1,
      slot = 0,
      eligibilityCertificate = eligibilityCertificate,
      operationalCertificate = operationalCertificate,
      metadata = ByteString.EMPTY,
      address = StakingAddress(zeroBytes(32))
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
      address = StakingAddress(zeroBytes(32))
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
    TetraScodecCodecs.consensusBlockHeaderCodec.encode(block.header).require.length + block.fullBody.transactions
      .map(ContainsImmutable.instances.ioTransactionImmutable.immutableBytes(_).value.size())
      .sum

}
