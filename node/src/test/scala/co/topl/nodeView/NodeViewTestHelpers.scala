package co.topl.nodeView

import co.topl.attestation.Address
import co.topl.consensus.NxtConsensus.State
import co.topl.consensus._
import co.topl.consensus.genesis.PrivateGenesis
import co.topl.modifier.block.Block
import co.topl.modifier.box.ArbitBox
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer}
import co.topl.nodeView.NodeViewTestHelpers.TestIn
import co.topl.nodeView.history.{History, InMemoryKeyValueStore, Storage}
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.{State, TokenBoxRegistry}
import co.topl.utils.{InMemoryKeyFileTestHelper, Int128, TestSettings, TimeProvider}
import org.scalatest.{BeforeAndAfterAll, Suite}

trait NodeViewTestHelpers extends BeforeAndAfterAll {
  self: Suite with InMemoryKeyFileTestHelper with TestSettings =>

  protected var genesisBlock: Block = _

  protected def nextBlock(parent: Block, nodeView: NodeView, forgerAddress: Address): Block = {
    val timestamp = parent.timestamp + 50000
    val arbitBox = LeaderElection
      .getEligibleBox(
        parent,
        keyRingCurve25519.addresses,
        timestamp,
        State(10000000, parent.difficulty, 0L, parent.height),
        nxtLeaderElection,
        nodeView.state
      )
      .toOption
      .get
    nextBlock(
      parent,
      arbitBox,
      nodeView.history.getTimestampsFrom(parent, 3),
      keyRingCurve25519.addresses.find(_.evidence == arbitBox.evidence).get
    )
  }

  protected def nextBlock(
    parent:             Block,
    arbitBox:           ArbitBox,
    previousTimestamps: Seq[TimeProvider.Time],
    rewardsAddress:     Address
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
        difficulty = nxtLeaderElection.calcNewBaseDifficulty(
          parent.height + 1,
          parent.difficulty,
          previousTimestamps :+ timestamp
        ),
        version = 1: Byte
      )(keyRingCurve25519.signWithAddress(rewardsAddress))
      .get
  }

  def genesisNodeView(): TestIn = {
    val historyStore = InMemoryKeyValueStore.empty()
    val stateStore = InMemoryKeyValueStore.empty()
    val tokenBoxStore = InMemoryKeyValueStore.empty()
    val nodeView = NodeView(
      History(settings, nxtLeaderElection, new Storage(historyStore)),
      State(
        settings,
        stateStore,
        keys => Some(new TokenBoxRegistry(tokenBoxStore, keys)),
        None
      ),
      MemPool.empty()
    )

    nodeView.history.append(genesisBlock, State(Int128(10000000), 1000000000000000000L, 0L, 0L))
    nodeView.state.applyModifier(genesisBlock)
    TestIn(nodeView, historyStore, stateStore, tokenBoxStore)
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    // A beforeAll step generates 3 keys.  We need 7 more to hit 10.
    keyRingCurve25519.generateNewKeyPairs(7)
    genesisBlock = PrivateGenesis(keyRingCurve25519.addresses, settings).formNewBlock._1
  }
}

object NodeViewTestHelpers {

  case class TestIn(
    nodeView:      NodeView,
    historyStore:  InMemoryKeyValueStore,
    stateStore:    InMemoryKeyValueStore,
    tokenBoxStore: InMemoryKeyValueStore
  )
}
