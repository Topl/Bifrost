package co.topl.nodeView

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.actor.typed.ActorRef
import akka.actor.typed.eventstream.EventStream
import akka.actor.{ActorRef => CActorRef, ActorSystem => CActorSystem}
import akka.io.{IO, Tcp}
import co.topl.attestation.PublicKeyPropositionCurve25519
import co.topl.consensus.KeyManager.{KeyView, StartupKeyView}
import co.topl.consensus.{ActorConsensusInterface, Forger, LocallyGeneratedBlock, NxtConsensus}
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.builder.{BoxSelectionAlgorithms, TransferBuilder, TransferRequests}
import co.topl.network.{NetworkControllerRef, PeerManager, PeerManagerRef}
import co.topl.nodeView.MemPoolAuditorSpec.TestInWithActor
import co.topl.nodeView.NodeViewTestHelpers.TestIn
import co.topl.nodeView.history.InMemoryKeyValueStore
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
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

    (() => timeProvider.time)
      .expects()
      .anyNumberOfTimes()
      .onCall(() => System.currentTimeMillis())

    genesisActorTest { testIn =>
      val addressA :: addressB :: _ = keyRingCurve25519.addresses.toList
      val polyTransfer = {
        val base =
          TransferBuilder
            .buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519](
              testIn.testIn.nodeView.state,
              TransferRequests.PolyTransferRequest(
                List(addressB),
                List(addressA -> 10),
                addressB,
                0,
                None
              ),
              BoxSelectionAlgorithms.All
            )
            .getOrThrow()
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
      val fstRawTx =
        TransferBuilder
          .buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519](
            testIn.testIn.nodeView.state,
            TransferRequests.PolyTransferRequest(
              List(addressB),
              List(addressA -> 10),
              changeAddress = addressB,
              fee = 666667,
              data = None
            ),
            BoxSelectionAlgorithms.All
          )
          .getOrThrow()

      val secRawTx = fstRawTx.copy(fee = 555555)

      val fstTx = fstRawTx.copy(attestation = keyRingCurve25519.generateAttestation(addressB)(fstRawTx.messageToSign))
      val secTx = secRawTx.copy(attestation = keyRingCurve25519.generateAttestation(addressB)(secRawTx.messageToSign))

      val probe = createTestProbe[LocallyGeneratedBlock]()
      system.eventStream.tell(EventStream.Subscribe(probe.ref))

      testIn.nodeViewHolderRef.tell(NodeViewHolder.ReceivableMessages.WriteTransactions(List(fstTx)))
      Thread.sleep(0.3.seconds.toMillis)
      testIn.testIn.nodeView.mempool.modifierById(fstTx.id).value shouldBe fstTx

      testIn.forgerRef.tell(Forger.ReceivableMessages.StartForging(system.ignoreRef))
      val forgedBlock: Block = probe.receiveMessage(1.seconds).block
      forgedBlock.transactions.map(_.id).contains(fstTx.id) shouldBe true
      Thread.sleep(1.seconds.toMillis)
      testIn.testIn.nodeView.mempool.modifierById(fstTx.id) shouldBe None

      testIn.forgerRef.tell(Forger.ReceivableMessages.StopForging(system.ignoreRef))
      testIn.nodeViewHolderRef.tell(NodeViewHolder.ReceivableMessages.WriteTransactions(List(secTx)))
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
      cSystem.actorOf(PeerManagerRef.props(testSettings, None), PeerManager.actorName)
    val networkControllerRef: CActorRef =
      cSystem.actorOf(NetworkControllerRef.props(testSettings, peerManagerRef, appContext, IO(Tcp)))
    val consensusStorageRef = spawn(
      NxtConsensus(
        settings,
        InMemoryKeyValueStore.empty()
      ),
      NxtConsensus.actorName
    )
    val consensusVariablesInterface = new ActorConsensusInterface(consensusStorageRef)
    val nodeViewHolderRef = spawn(
      NodeViewHolder(testSettings, consensusVariablesInterface, () => Future.successful(testIn.nodeView))
    )
    val memPoolAuditorRef = spawn(MemPoolAuditor(nodeViewHolderRef, networkControllerRef, testSettings))
    val forgerRef = spawn(
      Forger.behavior(
        blockGenerationDelay,
        minTransactionFee,
        forgeOnStartup = false,
        fetchKeyView,
        fetchStartupKeyView,
        new ActorNodeViewHolderInterface(nodeViewHolderRef),
        new ActorConsensusInterface(consensusStorageRef)
      )
    )

    val testInWithActor = TestInWithActor(testIn, nodeViewHolderRef, consensusStorageRef, memPoolAuditorRef, forgerRef)
    test(testInWithActor)
    testKit.stop(nodeViewHolderRef)
    testKit.stop(memPoolAuditorRef)
    testKit.stop(consensusStorageRef)
    testKit.stop(forgerRef)
  }
}

object MemPoolAuditorSpec {

  case class TestInWithActor(
    testIn:              TestIn,
    nodeViewHolderRef:   ActorRef[NodeViewHolder.ReceivableMessage],
    consensusStorageRef: ActorRef[NxtConsensus.ReceivableMessage],
    memPoolAuditorRef:   ActorRef[MemPoolAuditor.ReceivableMessage],
    forgerRef:           ActorRef[Forger.ReceivableMessage]
  )
}
