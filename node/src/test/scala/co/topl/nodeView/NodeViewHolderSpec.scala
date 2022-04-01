package co.topl.nodeView

import akka.actor.testkit.typed.FishingOutcome
import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.scaladsl.Behaviors
import akka.pattern.StatusReply
import cats.implicits._
import co.topl.attestation.keyManagement._
import co.topl.attestation.{PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.consensus._
import co.topl.crypto.signatures.Curve25519
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.{PolyTransfer, Transaction}
import co.topl.nodeView.history.{History, InMemoryKeyValueStore, MockImmutableBlockHistory, Storage}
import co.topl.nodeView.mempool.{MemPool, UnconfirmedTx}
import co.topl.nodeView.state.MockState
import co.topl.settings.Version
import co.topl.utils.TimeProvider.Time
import co.topl.utils.{NetworkPrefixTestHelper, NodeGenerators, TestSettings, TimeProvider}
import org.scalacheck.Gen
import org.scalamock.matchers.Matchers
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.ListMap
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

class NodeViewHolderSpec
    extends ScalaTestWithActorTestKit
    with ScalaCheckDrivenPropertyChecks
    with AnyFlatSpecLike
    with NodeGenerators
    with TestSettings
    with ValidBlockchainGenerator {

  val blockChainGen: Gen[GenesisHeadChain] =
    keyCurve25519Gen
      .map { keys =>
        implicit val keyFileCompanion: KeyfileCompanion[PrivateKeyCurve25519, KeyfileCurve25519] =
          KeyfileCurve25519Companion
        new KeyRing[PrivateKeyCurve25519, KeyfileCurve25519](None, Set(keys._1))
      }
      .flatMap(keyring =>
        validChainFromGenesis(
          keyring,
          GenesisProvider.Strategies.Generation(Version("0.0.1"), 10000000, 1000),
          ProtocolVersioner.default
        )(10.toByte)
      )

  behavior of "NodeViewHolder"

  it should "add syntactically valid transactions to the mempool when receiving write message" in {
    forAll(keyCurve25519Gen, addressGen, polyBoxGen, positiveLongGen) { (senderKeys, recipient, polyInput, timestamp) =>
      val unsignedPolyTransfer =
        PolyTransfer[PublicKeyPropositionCurve25519](
          IndexedSeq(senderKeys._2.address -> polyInput.nonce),
          IndexedSeq(recipient             -> polyInput.value),
          ListMap.empty,
          0,
          timestamp,
          None,
          minting = false
        )

      val signedPolyTransfer =
        unsignedPolyTransfer
          .copy(attestation =
            ListMap(
              senderKeys._2 -> SignatureCurve25519(
                Curve25519.sign(senderKeys._1.privateKey, unsignedPolyTransfer.messageToSign)
              )
            )
          )

      val consensusView =
        NxtConsensus.View(
          NxtConsensus.State(10000, 10000, 10000, 10000),
          new NxtLeaderElection(ProtocolVersioner.default),
          _ => Seq.empty
        )

      val nodeView =
        NodeView(
          MockImmutableBlockHistory.empty,
          MockState.empty,
          MemPool(TrieMap.empty)
        )

      implicit val timeProvider: TimeProvider = new TimeProvider {
        override def time: Time = timestamp + 1000
      }

      val consensusReader = MockConsensusReader(consensusView)(system.executionContext)

      val testProbe = createTestProbe[StatusReply[Option[Transaction.TX]]]()

      val testProbeActor =
        spawn(Behaviors.monitor[StatusReply[Option[Transaction.TX]]](testProbe.ref, Behaviors.ignore))

      val underTest =
        spawn(NodeViewHolder(TestSettings.defaultSettings, consensusReader, () => Future.successful(nodeView)))

      underTest.tell(NodeViewHolder.ReceivableMessages.WriteTransactions(List(signedPolyTransfer)))

      underTest.tell(
        NodeViewHolder.ReceivableMessages.Read(view => view.memPool.modifierById(signedPolyTransfer.id), testProbeActor)
      )

      testProbe.receiveMessage(2.seconds).getValue.get shouldBe signedPolyTransfer
    }
  }

  it should "remove transaction from the mempool when receiving eliminate message" in {
    forAll(polyTransferGen) { polyTransfer =>
      val consensusView =
        NxtConsensus.View(
          NxtConsensus.State(10000, 10000, 10000, 10000),
          new NxtLeaderElection(ProtocolVersioner.default),
          _ => Seq.empty
        )

      val currentTime = polyTransfer.timestamp + 1000

      val nodeView =
        NodeView(
          MockImmutableBlockHistory.empty,
          MockState.empty,
          MemPool(TrieMap(polyTransfer.id -> UnconfirmedTx(polyTransfer, currentTime)))
        )

      implicit val timeProvider: TimeProvider = new TimeProvider {
        override def time: Time = currentTime
      }

      val consensusReader = MockConsensusReader(consensusView)(system.executionContext)

      val testProbe = createTestProbe[StatusReply[Option[Transaction.TX]]]()

      val testProbeActor =
        spawn(Behaviors.monitor[StatusReply[Option[Transaction.TX]]](testProbe.ref, Behaviors.ignore))

      val underTest =
        spawn(NodeViewHolder(TestSettings.defaultSettings, consensusReader, () => Future.successful(nodeView)))

      underTest.tell(NodeViewHolder.ReceivableMessages.EliminateTransactions(List(polyTransfer.id)))

      underTest.tell(
        NodeViewHolder.ReceivableMessages.Read(view => view.memPool.modifierById(polyTransfer.id), testProbeActor)
      )

      testProbe.receiveMessage().getValue shouldBe None
    }
  }

  it should "place multiple orphan blocks into history tineProcessor when receiving the write blocks message" in {
    forAll(
      genesisBlockGen
    ) { genesisBlock =>
      val newBlocks = Gen.listOfN(5, blockCurve25519Gen(Some(Seq.empty))).sample.get
      val newBlockIds = newBlocks.map(_.id) // eval the block ids since they are lazy

      val consensusView =
        NxtConsensus.View(
          NxtConsensus.State(10000, 10000, 10000, 10000),
          new NxtLeaderElection(ProtocolVersioner.default),
          _ => Seq.empty
        )

      val existingHistory =
        History(TestSettings.defaultSettings, new Storage(new InMemoryKeyValueStore()))
          .append(genesisBlock, Seq.empty)
          .get
          ._1

      val nodeView =
        NodeView(
          existingHistory,
          MockState.empty,
          MemPool.empty()
        )

      val consensusReader = MockConsensusReader(consensusView)(system.executionContext)

      implicit val timeProvider: TimeProvider = new TimeProvider {
        override def time: Time = 0L
      }

      val testProbe = createTestProbe[StatusReply[Seq[ModifierId]]]()

      val testProbeActor =
        spawn(Behaviors.monitor[StatusReply[Seq[ModifierId]]](testProbe.ref, Behaviors.ignore))

      val underTest =
        spawn(NodeViewHolder(TestSettings.defaultSettings, consensusReader, () => Future.successful(nodeView)))

      val mapReadMessage = (view: ReadableNodeView) => newBlockIds.filter(view.history.contains)

      newBlocks.foreach(block => underTest.tell(NodeViewHolder.ReceivableMessages.WriteBlock(block)))

      val historyResult =
        NodeViewHolderSpec.searchInNodeView[ModifierId](
          blockIds => newBlockIds.forall(blockIds.contains),
          mapReadMessage,
          underTest,
          testProbeActor,
          testProbe
        )

      newBlockIds.foreach(historyResult.contains)
    }
  }

  it should "append multiple viable blocks to history when receiving the write blocks message" in {
    forAll(blockChainGen) { blockchain =>
      val consensusView =
        NxtConsensus.View(
          NxtConsensus.State(10000, 10000, 10000, 10000),
          new NxtLeaderElection(ProtocolVersioner.default),
          _ => Seq.empty
        )

      val genesisBlock = blockchain.head
      val newBlocks = blockchain.tail.toList

      val existingHistory =
        History(TestSettings.defaultSettings, new Storage(new InMemoryKeyValueStore()))
          .append(genesisBlock, Seq.empty)
          .get
          ._1

      val nodeView =
        NodeView(
          existingHistory,
          MockState.empty,
          MemPool.empty()
        )

      val consensusReader = MockConsensusReader(consensusView)(system.executionContext)

      implicit val timeProvider: TimeProvider = new TimeProvider {
        override def time: Time = 0L
      }

      val testProbe = createTestProbe[StatusReply[Seq[Block]]]()

      val testProbeActor =
        spawn(Behaviors.monitor[StatusReply[Seq[Block]]](testProbe.ref, Behaviors.ignore))

      val underTest =
        spawn(NodeViewHolder(TestSettings.defaultSettings, consensusReader, () => Future.successful(nodeView)))

      val mapReadMessage = (view: ReadableNodeView) => newBlocks.filter(view.history.contains)

      newBlocks.foreach(block => underTest.tell(NodeViewHolder.ReceivableMessages.WriteBlock(block)))

      val historyResult =
        NodeViewHolderSpec.searchInNodeView[Block](
          blockIds => newBlocks.forall(blockIds.contains),
          mapReadMessage,
          underTest,
          testProbeActor,
          testProbe
        )

      newBlocks.foreach(historyResult.contains)
    }
  }

  it should "cache a block received from the write blocks message and apply when the parent block is written" in {
    forAll(blockChainGen) { blockchain =>
      val consensusView =
        NxtConsensus.View(
          NxtConsensus.State(10000, 10000, 10000, 10000),
          new NxtLeaderElection(ProtocolVersioner.default),
          _ => Seq.empty
        )

      val genesisBlock = blockchain.head
      val firstNewBlock = blockchain.tail.tail.headOption.get
      val secondNewBlock = blockchain.tail.head

      val existingHistory =
        History(TestSettings.defaultSettings, new Storage(new InMemoryKeyValueStore()))
          .append(genesisBlock, Seq.empty)
          .get
          ._1

      val nodeView =
        NodeView(
          existingHistory,
          MockState.empty,
          MemPool.empty()
        )

      val consensusReader = MockConsensusReader(consensusView)(system.executionContext)

      implicit val timeProvider: TimeProvider = new TimeProvider {
        override def time: Time = 0L
      }

      val testProbe = createTestProbe[StatusReply[Seq[Block]]]()

      val testProbeActor =
        spawn(Behaviors.monitor[StatusReply[Seq[Block]]](testProbe.ref, Behaviors.ignore))

      val underTest =
        spawn(NodeViewHolder(TestSettings.defaultSettings, consensusReader, () => Future.successful(nodeView)))

      underTest.tell(NodeViewHolder.ReceivableMessages.WriteBlocks(List(firstNewBlock)))

      underTest.tell(
        NodeViewHolder.ReceivableMessages.Read(view => view.history.filter(_.id == firstNewBlock.id), testProbeActor)
      )

      testProbe.receiveMessage().getValue.isEmpty shouldBe true

      val mapReadMessage = (view: ReadableNodeView) => List(firstNewBlock, secondNewBlock).filter(view.history.contains)

      underTest.tell(NodeViewHolder.ReceivableMessages.WriteBlocks(List(secondNewBlock)))

      val historyResult =
        NodeViewHolderSpec.searchInNodeView[Block](
          blocks => blocks.contains(firstNewBlock),
          mapReadMessage,
          underTest,
          testProbeActor,
          testProbe
        )

      List(firstNewBlock, secondNewBlock).forall(historyResult.contains) shouldBe true
    }
  }
}

object NodeViewHolderSpec {

  /**
   * Uses a searching function to poll for the existence of blocks in the node view.
   * @param search a function which determines if the desired blocks exist in the current node view
   * @param nodeViewHolder thenode view holder to check
   * @param testProbeActor an actor that can be used as the reply to for sending messages to the node view
   * @param testProbe a test probe which can be used to check for response messages from the node view
   * @return the set of blocks in the node view if the search ever matched
   */
  def searchInNodeView[T](
    search:         Seq[T] => Boolean,
    mapView:        ReadableNodeView => Seq[T],
    nodeViewHolder: ActorRef[NodeViewHolder.ReceivableMessage],
    testProbeActor: ActorRef[StatusReply[Seq[T]]],
    testProbe:      TestProbe[StatusReply[Seq[T]]]
  ): Seq[T] = {
    // A "creative" solution for waiting on the Node View Holder to write all the blocks in its cache to history.
    // If the Read message comes back without the new blocks, ignore it and try sending another read message.
    // This block will return no messages if the history was never updated correctly.
    nodeViewHolder.tell(
      NodeViewHolder.ReceivableMessages.Read(mapView, testProbeActor)
    )

    testProbe
      .fishForMessage(10000.millis) {
        case StatusReply.Success(values: Seq[T]) if search(values) =>
          FishingOutcome.Complete
        case _ =>
          nodeViewHolder.tell(
            NodeViewHolder.ReceivableMessages.Read(mapView, testProbeActor)
          )
          FishingOutcome.ContinueAndIgnore
      }
      .headOption
      .toList
      .flatMap(result => if (result.isSuccess) result.getValue else Seq.empty)
  }

}
