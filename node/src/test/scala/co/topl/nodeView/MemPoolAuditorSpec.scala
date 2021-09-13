package co.topl.nodeView

import akka.Done
import akka.actor.testkit.typed.scaladsl.{ManualTime, ScalaTestWithActorTestKit}
import akka.actor.typed.ActorRef
import akka.actor.typed.eventstream.EventStream
import akka.actor.{ActorRef => CActorRef, ActorSystem => CActorSystem}
import akka.io.{IO, Tcp}
import co.topl.attestation.PublicKeyPropositionCurve25519
import co.topl.consensus.{Forger, LocallyGeneratedBlock}
import co.topl.consensus.KeyManager.{KeyView, StartupKeyView}
import co.topl.modifier.box.SimpleValue
import co.topl.modifier.transaction.PolyTransfer
import co.topl.network.{NetworkControllerRef, PeerManager, PeerManagerRef}
import co.topl.nodeView.MemPoolAuditorSpec.TestInWithActor
import co.topl.nodeView.NodeViewTestHelpers.TestIn
import co.topl.utils.{InMemoryKeyFileTestHelper, Int128, TestSettings, TimeProvider}
import org.scalamock.scalatest.MockFactory
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpecLike

import scala.concurrent.Future
import scala.concurrent.duration._

class MemPoolAuditorSpec
    extends ScalaTestWithActorTestKit
    with AnyFlatSpecLike
    with TestSettings
    with InMemoryKeyFileTestHelper
    with NodeViewTestHelpers
    with MockFactory
    with OptionValues {

  behavior of "MemPoolAuditor"

  private val blockGenerationDelay = 1.seconds
  private val minTransactionFee: Int128 = 0

  it should "Tell nodeViewHolder to eliminate transactions that stayed mem pool longer than mempoolTimeout" in {

    implicit val timeProvider: TimeProvider = mock[TimeProvider]

    (() => timeProvider.time)
      .expects()
      .anyNumberOfTimes()
      .onCall(() => System.currentTimeMillis())

    genesisActorTest { testIn =>
      val addressA :: addressB :: _ = keyRingCurve25519.addresses.toList
      val polyTransfer = {
        val base = PolyTransfer
          .createRaw[PublicKeyPropositionCurve25519](
            testIn.testIn.nodeView.state,
            toReceive = IndexedSeq((addressA, SimpleValue(10))),
            sender = IndexedSeq(addressB),
            changeAddress = addressB,
            fee = 0,
            data = None
          )
          .get
        base.copy(attestation = keyRingCurve25519.generateAttestation(addressB)(base.messageToSign))
      }
      val transactions = List(polyTransfer)
      testIn.nodeViewHolderRef.tell(NodeViewHolder.ReceivableMessages.WriteTransactions(transactions))
      Thread.sleep(0.5.seconds.toMillis)
      testIn.testIn.nodeView.mempool.modifierById(transactions.head.id).value shouldBe transactions.head
      Thread.sleep(1.seconds.toMillis)
      // Using genesisBlock since the memPoolAuditor doesn't care which specific block passed the checks
      system.eventStream.tell(EventStream.Publish(NodeViewHolder.Events.SemanticallySuccessfulModifier(genesisBlock)))
      Thread.sleep(0.5.seconds.toMillis)
      testIn.testIn.nodeView.mempool.modifierById(transactions.head.id) shouldBe None
    }
  }

  it should "Invalidate transactions with invalid input boxes after state changes" in {

    implicit val timeProvider: TimeProvider = mock[TimeProvider]

    (() => timeProvider.time)
      .expects()
      .anyNumberOfTimes()
      .onCall(() => System.currentTimeMillis())

    genesisActorTest { testIn =>
      val addressA :: addressB :: _ = keyRingCurve25519.addresses.toList
      val fstRawTx = PolyTransfer
        .createRaw[PublicKeyPropositionCurve25519](
          testIn.testIn.nodeView.state,
          toReceive = IndexedSeq((addressA, SimpleValue(10))),
          sender = IndexedSeq(addressB),
          changeAddress = addressB,
          fee = 666667,
          data = None
        )
        .get
      val secRawTx = fstRawTx.copy(fee = 555555)

      val fstTx = fstRawTx.copy(attestation = keyRingCurve25519.generateAttestation(addressB)(fstRawTx.messageToSign))
      val secTx = secRawTx.copy(attestation = keyRingCurve25519.generateAttestation(addressB)(secRawTx.messageToSign))

      val probe = createTestProbe[LocallyGeneratedBlock]()
      system.eventStream.tell(EventStream.Subscribe(probe.ref))

      testIn.nodeViewHolderRef.tell(NodeViewHolder.ReceivableMessages.WriteTransactions(List(fstTx)))
      system.eventStream.tell(EventStream.Publish(NodeViewHolder.Events.SemanticallySuccessfulModifier(genesisBlock)))
      Thread.sleep(0.3.seconds.toMillis)
      testIn.testIn.nodeView.mempool.modifierById(fstTx.id).value shouldBe fstTx

      testIn.forgerRef.tell(Forger.ReceivableMessages.StartForging(system.ignoreRef))
      val forgedBlock = probe.receiveMessage(1.seconds).block
      Thread.sleep(1.seconds.toMillis)
      testIn.testIn.nodeView.mempool.modifierById(fstTx.id) shouldBe None

      testIn.forgerRef.tell(Forger.ReceivableMessages.StopForging(system.ignoreRef))
      testIn.nodeViewHolderRef.tell(NodeViewHolder.ReceivableMessages.WriteTransactions(List(secTx)))
      system.eventStream.tell(EventStream.Publish(NodeViewHolder.Events.SemanticallySuccessfulModifier(genesisBlock)))
      Thread.sleep(0.5.seconds.toMillis)
      testIn.testIn.nodeView.mempool.modifierById(fstTx.id) shouldBe None
    }
  }

  private def genesisActorTest(test: TestInWithActor => Unit)(implicit timeProvider: TimeProvider): Unit = {
    implicit val cSystem: CActorSystem = akka.actor.ActorSystem("MPASpec")
    val testSettings = settings.copy(application =
      // Setting `mempoolTimeout` to 1s
      settings.application.copy(mempoolTimeout = 1.seconds)
    )

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

    val fetchStartupKeyView = mockFunction[Future[StartupKeyView]]
    fetchStartupKeyView
      .expects()
      .once()
      .returning(Future.successful(StartupKeyView(keyView.addresses, keyView.rewardAddr)))

    val testIn = genesisNodeView()

    val peerManagerRef: CActorRef =
      cSystem.actorOf(PeerManagerRef.props(testSettings, appContext), PeerManager.actorName)
    val networkControllerRef: CActorRef =
      cSystem.actorOf(NetworkControllerRef.props(testSettings, peerManagerRef, appContext, IO(Tcp)))

    val nodeViewHolderRef = spawn(NodeViewHolder(testSettings, () => Future.successful(testIn.nodeView)))
    val memPoolAuditorRef = spawn(MemPoolAuditor(nodeViewHolderRef, networkControllerRef, testSettings))
    val forgerRef = spawn(
      Forger.behavior(
        blockGenerationDelay,
        minTransactionFee,
        forgeOnStartup = false,
        fetchKeyView,
        fetchStartupKeyView,
        new ActorNodeViewHolderInterface(nodeViewHolderRef)
      )
    )

    val testInWithActor = TestInWithActor(testIn, nodeViewHolderRef, memPoolAuditorRef, forgerRef)
    test(testInWithActor)
    testKit.stop(nodeViewHolderRef)
    testKit.stop(memPoolAuditorRef)
  }
}

object MemPoolAuditorSpec {

  case class TestInWithActor(
    testIn:            TestIn,
    nodeViewHolderRef: ActorRef[NodeViewHolder.ReceivableMessage],
    memPoolAuditorRef: ActorRef[MemPoolAuditor.ReceivableMessage],
    forgerRef:         ActorRef[Forger.ReceivableMessage]
  )
}
