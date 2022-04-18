package co.topl.consensus

import akka.Done
import akka.actor.testkit.typed.scaladsl.{LoggingTestKit, ManualTime, ScalaTestWithActorTestKit}
import akka.actor.typed.eventstream.EventStream
import cats.data.EitherT
import cats.implicits._
import co.topl.attestation.Address
import co.topl.consensus.KeyManager.KeyView
import co.topl.consensus.KeyManager.{KeyView, StartupKeyView}
import co.topl.modifier.ProgramId
import co.topl.modifier.block.Block
import co.topl.modifier.box.{ArbitBox, SimpleValue}
import co.topl.modifier.transaction.Transaction
import co.topl.network.BifrostSyncInfo
import co.topl.nodeView.history.{HistoryReader, InMemoryKeyValueStore}
import co.topl.nodeView.mempool.{MemPoolReader, UnconfirmedTx}
import co.topl.nodeView.state.StateReader
import co.topl.nodeView.{NodeViewHolderInterface, NodeViewReader, ReadableNodeView}
import co.topl.utils._
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.{Inspectors, OptionValues}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

class ForgerSpec
    extends ScalaTestWithActorTestKit(ManualTime.config.withFallback(TestSettings.defaultConfig))
    with AnyFlatSpecLike
    with InMemoryKeyRingTestHelper
    with TestSettings
    with MockFactory
    with OptionValues
    with Inspectors
    with CommonGenerators {

  behavior of "Forger"

  implicit def ec: ExecutionContext = system.executionContext

  private val blockGenerationDelay = 1.seconds
  private val minTransactionFee: Int128 = 0

  // ManualTime allows us to manually control the ActorSystem scheduler, which makes this test more predictable on
  // limited-hardware machines
  private val manualTime: ManualTime = ManualTime()

  it should "attempt to generate a new block every 'blockGenerationDelay' seconds" in {

    val parentBlock = blockCurve25519Gen.pureApply(Gen.Parameters.default, Seed.random())

    implicit val timeProvider: TimeProvider = mock[TimeProvider]

    val rewardsAddress = keyRingCurve25519.addresses.head
    val keyView =
      KeyView(
        keyRingCurve25519.addresses,
        Some(rewardsAddress),
        keyRingCurve25519.signWithAddress,
        keyRingCurve25519.lookupPublicKey
      )

    val fetchKeyView = mockFunction[Future[KeyView]]
    fetchKeyView
      .expects()
      .anyNumberOfTimes()
      .returning(Future.successful(keyView))

    val nodeView = ReadableNodeView(
      mock[HistoryReader[Block, BifrostSyncInfo]],
      mock[StateReader[ProgramId, Address]],
      mock[MemPoolReader[Transaction.TX]]
    )
    val reader = mock[NodeViewReader]
    (reader
      .withNodeView[Forge](_: ReadableNodeView => Forge))
      .expects(*)
      .anyNumberOfTimes()
      .onCall((f: (ReadableNodeView) => Forge) =>
        EitherT.pure[Future, NodeViewHolderInterface.ReadFailure](f(nodeView))
      )

    (() => nodeView.history.height)
      .expects()
      .anyNumberOfTimes()
      .returning(parentBlock.height)

    (nodeView.memPool
      .take[Int128](_: Int)(_: UnconfirmedTx[Transaction.TX] => Int128)(_: Ordering[Int128]))
      .expects(*, *, *)
      .anyNumberOfTimes()
      .returning(Nil)

    (() => timeProvider.time)
      .expects()
      .anyNumberOfTimes()
      // Big time delta allows LeaderElection to find an eligible box
      .returning(Long.MaxValue)

    (() => nodeView.history.bestBlock)
      .expects()
      .anyNumberOfTimes()
      .returning(parentBlock)

    (nodeView.history
      .getTimestampsFrom(_: Block, _: Long))
      .expects(parentBlock, 3)
      .anyNumberOfTimes()
      .returning(Vector(parentBlock.timestamp))

    (nodeView.state
      .getTokenBoxes(_: Address))
      .expects(*)
      .anyNumberOfTimes()
      .onCall((a: Address) => Some(List(ArbitBox(a.evidence, nonce = Long.MaxValue, value = SimpleValue(1)))))

    val probe = createTestProbe[LocallyGeneratedBlock]()
    val initializedProbe = createTestProbe[Done]()

    // The probe reads [LocallyGeneratedBlock] messages from the EventStream
    system.eventStream.tell(EventStream.Subscribe(probe.ref))
    var blocks: List[Block] = Nil

    val newBlockCount = 4

    LoggingTestKit.info("Forger is initialized").expect {
      LoggingTestKit.info("Starting forging").expect {
        LoggingTestKit.debug("New local block").withOccurrences(newBlockCount + 1).expect {
          val consensusStorageRef =
            spawn(
              NxtConsensus(
                settings,
                InMemoryKeyValueStore.empty
              ),
              NxtConsensus.actorName
            )
          val consensusInterface = new ActorConsensusInterface(consensusStorageRef)
          val forgerRef = spawn(
            Forger.behavior(
              blockGenerationDelay,
              minTransactionFee,
              forgeOnStartup = false,
              fetchKeyView,
              reader,
              consensusInterface
            )
          )

          // updating the consensus since we don't initialize the nodeViewHolder which sets the default consensus value
          consensusInterface.update(
            parentBlock.id,
            NxtConsensus
              .StateUpdate(Some(Int128(10000000)), Some(parentBlock.difficulty), None, Some(parentBlock.height))
          )
          forgerRef.tell(Forger.ReceivableMessages.StartForging(initializedProbe.ref))
          initializedProbe.expectMessage(2.seconds, Done)

          for (_ <- 0 until newBlockCount) {
            blocks :+= probe.receiveMessage(1.seconds).block
            // Manually advance the clock forward a delay amount
            manualTime.timePasses(blockGenerationDelay)
          }

        }
        blocks should have size newBlockCount
        blocks.distinct should contain theSameElementsInOrderAs blocks
      }
    }
  }

  it should "continue to attempt blocks after an unexpected exception" in {

    implicit val timeProvider: TimeProvider = mock[TimeProvider]

    (() => timeProvider.time)
      .expects()
      .never()

    val rewardsAddress = keyRingCurve25519.addresses.head
    val keyView =
      KeyView(
        keyRingCurve25519.addresses,
        Some(rewardsAddress),
        keyRingCurve25519.signWithAddress,
        keyRingCurve25519.lookupPublicKey
      )

    val fetchKeyView = mockFunction[Future[KeyView]]
    fetchKeyView
      .expects()
      .anyNumberOfTimes()
      .returning(Future.failed(new Exception("Expected failure")))

    val reader = mock[NodeViewReader]

    val probe = createTestProbe[LocallyGeneratedBlock]()

    system.eventStream.tell(EventStream.Subscribe(probe.ref))

    val newBlockCount = 4

    val consensusStorageRef =
      spawn(
        NxtConsensus(
          settings,
          InMemoryKeyValueStore.empty
        ),
        NxtConsensus.actorName
      )

    val forgerRef = spawn(
      Forger.behavior(
        blockGenerationDelay,
        minTransactionFee,
        forgeOnStartup = false,
        fetchKeyView,
        reader,
        new ActorConsensusInterface(consensusStorageRef)
      )
    )

    forgerRef.tell(Forger.ReceivableMessages.StartForging(system.ignoreRef))

    for (_ <- 0 until newBlockCount) {
      probe.expectNoMessage(1.seconds)
      // Manually advance the clock forward a delay amount
      manualTime.timePasses(blockGenerationDelay)
    }
  }

  it should "fail if private forging does not specify a rewards address" in {
    implicit val timeProvider: TimeProvider = mock[TimeProvider]

    val keyView =
      KeyView(keyRingCurve25519.addresses, None, keyRingCurve25519.signWithAddress, keyRingCurve25519.lookupPublicKey)

    val fetchKeyView = mockFunction[Future[KeyView]]
    fetchKeyView
      .expects()
      .once()
      .returning(Future.successful(keyView))

    val reader = mock[NodeViewReader]

    LoggingTestKit.error("Forging requires a rewards address").expect {
      val consensusStorageRef =
        spawn(
          NxtConsensus(
            settings,
            InMemoryKeyValueStore.empty
          ),
          NxtConsensus.actorName
        )
      val forgerRef = spawn(
        Forger.behavior(
          blockGenerationDelay,
          minTransactionFee,
          forgeOnStartup = true,
          fetchKeyView,
          reader,
          new ActorConsensusInterface(consensusStorageRef)
        )
      )
      createTestProbe().expectTerminated(forgerRef)
    }
  }

}
