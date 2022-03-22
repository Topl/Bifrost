package co.topl.nodeView

import co.topl.attestation.Address
import co.topl.consensus._
import co.topl.modifier.block.Block
import co.topl.modifier.box.ArbitBox
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer}
import co.topl.nodeView.NodeViewTestHelpers.TestIn
import co.topl.nodeView.history.{History, InMemoryKeyValueStore, Storage, TineProcessor}
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.{ProgramBoxRegistry, State, TokenBoxRegistry}
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.{InMemoryKeyRingTestHelper, TimeProvider}
import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.collection.AbstractIterator

trait NodeViewTestHelpers extends BeforeAndAfterAll with InMemoryKeyRingTestHelper {
  self: Suite =>

  def genesisNodeViewTestInputs(
    genesisConsensusView:       NxtConsensus.Genesis
  )(implicit protocolVersioner: ProtocolVersioner): TestIn = {
    val historyStore = InMemoryKeyValueStore.empty()
    val stateStore = InMemoryKeyValueStore.empty()
    val tokenBoxStore = InMemoryKeyValueStore.empty()

    val state =
      State(genesisConsensusView.block.id, stateStore, Some(new TokenBoxRegistry(tokenBoxStore, None)), None, None)
    val history = new History(new Storage(historyStore), TineProcessor(1024))

    val nodeView = NodeView(
      history,
      state,
      MemPool.empty()
    )

    nodeView.state.applyModifier(genesisConsensusView.block)
    nodeView.history.append(genesisConsensusView.block, Seq())

    TestIn(nodeView, historyStore, stateStore, tokenBoxStore, genesisConsensusView)
  }

  def generateState(genesisBlock: Block)(implicit networkPrefix: NetworkPrefix): State = {
    val tokenBoxRegistry = new TokenBoxRegistry(InMemoryKeyValueStore.empty(), None)
    val programBoxRegistry = new ProgramBoxRegistry(InMemoryKeyValueStore.empty())
    val state =
      State(genesisBlock.id, InMemoryKeyValueStore.empty(), Some(tokenBoxRegistry), Some(programBoxRegistry), None)
    state.applyModifier(genesisBlock).get
  }

  def generateHistory(
    genesisBlock:           Block
  )(implicit networkPrefix: NetworkPrefix, protocolVersioner: ProtocolVersioner): History = {
    val storage = new Storage(new InMemoryKeyValueStore)
    val validators = Seq()
    var history = new History(storage, TineProcessor(1024))
    history = history.append(genesisBlock, validators).get._1
    assert(history.modifierById(genesisBlock.id).isDefined)
    history
  }

//  protected def nextBlock(parent: Block, nodeView: NodeView): Block = {
//    val timestamp = parent.timestamp + 50000
//    val leaderElection = new NxtLeaderElection(protocolVersioner)
//
//    val arbitBox = NxtLeaderElection
//      .getEligibleBox(
//        parent,
//        keyRingCurve25519.addresses,
//        timestamp,
//        NxtConsensus.State(10000000, parent.difficulty, 0L, parent.height),
//        leaderElection
//      )
//      .toOption
//      .get
//
//    nextBlock(
//      parent,
//      arbitBox,
//      nodeView.history.getTimestampsFrom(parent, 3),
//      keyRingCurve25519.addresses.find(_.evidence == arbitBox.evidence).get,
//      leaderElection
//    )
//  }

  protected def nextBlock(
    parent:             Block,
    arbitBox:           ArbitBox,
    previousTimestamps: Seq[TimeProvider.Time],
    rewardsAddress:     Address,
    leaderElection:     NxtLeaderElection
  ): Block = {
    val timestamp = parent.timestamp + 50000
    val rewards = {
      val base = Rewards(Nil, rewardsAddress, parent.id, timestamp, 0L).get
      base.map {
        case b: PolyTransfer[_] =>
          b.copy(attestation = keyRingCurve25519.generateAttestation(keyRingCurve25519.addresses)(b.messageToSign))
        case a: ArbitTransfer[_] =>
          a.copy(attestation = keyRingCurve25519.generateAttestation(keyRingCurve25519.addresses)(a.messageToSign))
      }
    }
    Block
      .createAndSign(
        parent.id,
        timestamp = timestamp,
        txs = rewards,
        generatorBox = arbitBox,
        publicKey = keyRingCurve25519.lookupPublicKey(rewardsAddress).get,
        height = parent.height + 1,
        difficulty = leaderElection.calcNewBaseDifficulty(
          parent.height + 1,
          parent.difficulty,
          previousTimestamps :+ timestamp
        ),
        version = 1: Byte
      )(keyRingCurve25519.signWithAddress(rewardsAddress))
      .get
  }

  def generateBlockExtensions(
    genesisBlock:   Block,
    previousBlocks: List[Block],
    forgerAddress:  Address,
    leaderElection: NxtLeaderElection
  ): Iterator[Block] =
    new AbstractIterator[Block] {

      // Because the reward fee is 0, the genesis arbit box is never destroyed during forging, so we can re-use it
      private val arbitBox =
        previousBlocks.last.transactions
          .flatMap(_.newBoxes)
          .collectFirst { case a: ArbitBox if a.evidence == forgerAddress.evidence => a }
          .get
      private var previous3Blocks: List[Block] = previousBlocks.takeRight(3)

      override def hasNext: Boolean = true

      override def next(): Block =
        if (previous3Blocks.isEmpty) {
          previous3Blocks = List(genesisBlock)
          genesisBlock
        } else {
          val newBlock = nextBlock(
            previous3Blocks.last,
            arbitBox,
            previous3Blocks.map(_.timestamp),
            forgerAddress,
            leaderElection
          )
          previous3Blocks = (previous3Blocks :+ newBlock).takeRight(3)
          newBlock
        }
    }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    // A beforeAll step generates 3 keys.  We need 7 more to hit 10.
    keyRingCurve25519.generateNewKeyPairs(7)
  }
}

object NodeViewTestHelpers {

  case class TestIn(
    nodeView:      NodeView,
    historyStore:  InMemoryKeyValueStore,
    stateStore:    InMemoryKeyValueStore,
    tokenBoxStore: InMemoryKeyValueStore,
    genesisView:   NxtConsensus.Genesis
  )
}
