package co.topl.consensus

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.actor.typed.eventstream.EventStream
import cats.data.EitherT
import cats.implicits._
import co.topl.attestation.Address
import co.topl.consensus.KeyManager.{KeyView, StartupKeyView}
import co.topl.modifier.block.Block
import co.topl.modifier.box.{ArbitBox, ProgramId, SimpleValue}
import co.topl.modifier.transaction.Transaction
import co.topl.network.message.BifrostSyncInfo
import co.topl.nodeView.history.HistoryReader
import co.topl.nodeView.mempool.{MemPoolReader, UnconfirmedTx}
import co.topl.nodeView.state.StateReader
import co.topl.nodeView.{NodeViewHolderInterface, NodeViewReader, NodeViewTestHelpers, ReadableNodeView}
import co.topl.utils._
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.{Inspectors, OptionValues}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

class ForgerSpec
    extends ScalaTestWithActorTestKit
    with AnyFlatSpecLike
    with TestSettings
    with InMemoryKeyFileTestHelper
    with NodeViewTestHelpers
    with MockFactory
    with OptionValues
    with Inspectors
    with CommonGenerators {

  behavior of "Forger"

  implicit def ec: ExecutionContext = system.executionContext

  private val specialSettings =
    settings.copy(forging = settings.forging.copy(forgeOnStartup = false, blockGenerationDelay = 1.seconds))

  it should "generate a new block every 'blockGenerationDelay' seconds" in {

    val parentBlock = blockGen.pureApply(Gen.Parameters.default, Seed.random())

    implicit val timeProvider: TimeProvider = mock[TimeProvider]

    val rewardsAddress = keyRing.addresses.head
    val keyView =
      KeyView(keyRing.addresses, Some(rewardsAddress), keyRing.signWithAddress, keyRing.lookupPublicKey)

    val fetchKeyView = mockFunction[Future[KeyView]]
    fetchKeyView
      .expects()
      .anyNumberOfTimes()
      .returning(Future.successful(keyView))

    val fetchStartupKeyView = mockFunction[Future[StartupKeyView]]
    fetchStartupKeyView
      .expects()
      .once()
      .returning(Future.successful(StartupKeyView(keyView.addresses, keyView.rewardAddr)))

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
//      .onCall { case f: Function1[ReadableNodeView, Forge] =>
      .onCall((f: Function1[ReadableNodeView, Forge]) =>
        EitherT.pure[Future, NodeViewHolderInterface.ReadFailure](f(nodeView))
      )
//      }

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

    val behavior = Forger.behavior(
      specialSettings,
      appContext,
      fetchKeyView,
      fetchStartupKeyView,
      reader
    )

    val probe = createTestProbe[LocallyGeneratedBlock]()

    system.eventStream.tell(EventStream.Subscribe(probe.ref))

    val ref = spawn(behavior)

    ref.tell(Forger.ReceivableMessages.StartForging(system.ignoreRef))
    var blocks: List[Block] = Nil
    for (_ <- 0 to 3) {
      blocks :+= probe.receiveMessage().block
      Thread.sleep(specialSettings.forging.blockGenerationDelay.toMillis)
    }

    blocks.distinct should contain theSameElementsInOrderAs blocks
  }

  it should "fail if private forging does not specify a rewards address" in {
    implicit val timeProvider: TimeProvider = mock[TimeProvider]
    val keyView =
      KeyView(keyRing.addresses, None, keyRing.signWithAddress, keyRing.lookupPublicKey)

    val fetchKeyView = mockFunction[Future[KeyView]]
    fetchKeyView
      .expects()
      .anyNumberOfTimes()
      .returning(Future.successful(keyView))

    val fetchStartupKeyView = mockFunction[Future[StartupKeyView]]
    fetchStartupKeyView
      .expects()
      .once()
      .returning(Future.successful(StartupKeyView(keyView.addresses, keyView.rewardAddr)))
    val reader = mock[NodeViewReader]
    val behavior = Forger.behavior(
      specialSettings,
      appContext,
      fetchKeyView,
      fetchStartupKeyView,
      reader
    )

    val ref = spawn(behavior)
    createTestProbe().expectTerminated(ref)
  }

}
