package co.topl.nodeView

import akka.actor.testkit.typed.FishingOutcome
import akka.actor.testkit.typed.scaladsl.{ScalaTestWithActorTestKit, TestProbe}
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.Behaviors
import akka.pattern.StatusReply
import cats.implicits._
import co.topl.attestation.keyManagement._
import co.topl.attestation.{PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.consensus._
import co.topl.crypto.Signature
import co.topl.crypto.signing.Curve25519
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Sized
import co.topl.models.{Bytes, SecretKeys}
import co.topl.modifier.block.Block
import co.topl.modifier.block.Block.toComponents
import co.topl.modifier.transaction.{PolyTransfer, Transaction}
import co.topl.nodeView.history.{History, InMemoryKeyValueStore, MockImmutableBlockHistory, Storage}
import co.topl.nodeView.mempool.{MemPool, UnconfirmedTx}
import co.topl.nodeView.state.MockState
import co.topl.settings.Version
import co.topl.utils.TimeProvider.Time
import co.topl.utils.{NodeGenerators, TestSettings, TimeProvider}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.ListMap
import scala.concurrent.Future
import scala.concurrent.duration._

class NodeViewHolderSpec
    extends ScalaTestWithActorTestKit
    with ScalaCheckDrivenPropertyChecks
    with AnyFlatSpecLike
    with NodeGenerators
    with TestSettings
    with ValidBlockchainGenerator {

  val blockChainGen: Gen[GenesisHeadChain] =
    keyCurve25519FastGen
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
    forAll(keyCurve25519FastGen, addressGen, polyBoxGen, positiveLongGen) {
      (senderKeys, recipient, polyInput, timestamp) =>
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
                  Signature(
                    Curve25519.instance
                      .sign(
                        SecretKeys.Curve25519(Sized.strictUnsafe(Bytes(senderKeys._1.privateKey.value))),
                        Bytes(unsignedPolyTransfer.messageToSign)
                      )
                      .bytes
                      .data
                      .toArray
                  )
                )
              )
            )

        val consensusView =
          NxtConsensus.State(10000, 10000)

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
          NodeViewHolder.ReceivableMessages
            .Read(view => view.memPool.modifierById(signedPolyTransfer.id), testProbeActor)
        )

        testProbe.receiveMessage(2.seconds).getValue.get shouldBe signedPolyTransfer
    }
  }

  it should "remove transaction from the mempool when receiving eliminate message" in {
    forAll(polyTransferGen) { polyTransfer =>
      val consensusView =
        NxtConsensus.State(10000, 10000)

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

  it should "append a valid block to the main tine when receiving the WriteBlocks message" in {
    forAll(blockChainGen) { blockchain =>
      val genesis = blockchain.head
      val newBlock = blockchain.tail.head

      val existingHistory =
        History(TestSettings.defaultSettings, new Storage(new InMemoryKeyValueStore()))
          .append(genesis.block, Seq.empty)
          .get
          ._1

      val nodeView =
        NodeView(
          existingHistory,
          MockState.empty,
          MemPool.empty()
        )

      val consensusReader = MockConsensusReader(genesis.state)(system.executionContext)

      implicit val timeProvider: TimeProvider = new TimeProvider {
        override def time: Time = 0L
      }

      val testProbe = createTestProbe[StatusReply[Seq[Block]]]()

      val testProbeActor =
        spawn(Behaviors.monitor[StatusReply[Seq[Block]]](testProbe.ref, Behaviors.ignore))

      val underTest =
        spawn(NodeViewHolder(TestSettings.defaultSettings, consensusReader, () => Future.successful(nodeView)))

      /*
        Sending WriteBlocks kicks off a lot of background processes that should end with the block being
        available in history.
       */
      underTest.tell(NodeViewHolder.ReceivableMessages.WriteBlocks(List(newBlock)))

      val historyResult =
        NodeViewHolderSpec.searchInNodeView[Block](
          (blocks: Seq[Block]) => blocks.nonEmpty,
          (view: ReadableNodeView) => view.history.filter(_.id == newBlock.id),
          underTest,
          testProbeActor,
          testProbe
        )

      historyResult should contain(newBlock)

    }
  }

  it should "append multiple viable blocks to history when receiving the write blocks message" in {
    forAll(blockChainGen) { blockchain =>
      val genesis = blockchain.head
      val newBlocks = blockchain.tail.toList

      val existingHistory =
        History(TestSettings.defaultSettings, new Storage(new InMemoryKeyValueStore()))
          .append(genesis.block, Seq.empty)
          .get
          ._1

      val nodeView =
        NodeView(
          existingHistory,
          MockState.empty,
          MemPool.empty()
        )

      val consensusReader = MockConsensusReader(genesis.state)(system.executionContext)

      implicit val timeProvider: TimeProvider = new TimeProvider {
        override def time: Time = 0L
      }

      val testProbe = createTestProbe[StatusReply[Seq[Block]]]()

      val testProbeActor =
        spawn(Behaviors.monitor[StatusReply[Seq[Block]]](testProbe.ref, Behaviors.ignore))

      val underTest =
        spawn(NodeViewHolder(TestSettings.defaultSettings, consensusReader, () => Future.successful(nodeView)))

      underTest.tell(NodeViewHolder.ReceivableMessages.WriteBlocks(newBlocks))

      val historyResult =
        NodeViewHolderSpec.searchInNodeView[Block](
          (blocks: Seq[Block]) => newBlocks.forall(blocks.contains),
          (view: ReadableNodeView) => newBlocks.filter(view.history.contains),
          underTest,
          testProbeActor,
          testProbe
        )

      newBlocks.foreach(historyResult.contains)
    }
  }

  it should "cache a block received from the write blocks message and apply when the parent block is written" in {
    forAll(blockChainGen) { blockchain =>
      val genesis = blockchain.head
      val firstNewBlock = blockchain.tail.tail.headOption.get
      val secondNewBlock = blockchain.tail.head

      val existingHistory =
        History(TestSettings.defaultSettings, new Storage(new InMemoryKeyValueStore()))
          .append(genesis.block, Seq.empty)
          .get
          ._1

      val nodeView =
        NodeView(
          existingHistory,
          MockState.empty,
          MemPool.empty()
        )

      val consensusReader = MockConsensusReader(genesis.state)(system.executionContext)

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
        NodeViewHolder.ReceivableMessages.Read(_.history.filter(_.id == firstNewBlock.id), testProbeActor)
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

      historyResult should contain(firstNewBlock)
      historyResult should contain(secondNewBlock)
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
      .fishForMessage(3.seconds) {
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
