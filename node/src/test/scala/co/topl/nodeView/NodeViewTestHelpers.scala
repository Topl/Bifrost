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

  protected case class AccessibleHistory(history: History, storage: InMemoryKeyValueStore)

  protected case class AccessibleState(
    state:      State,
    stateStore: InMemoryKeyValueStore,
    tbrStore:   InMemoryKeyValueStore,
    pbrStore:   InMemoryKeyValueStore
  )

  def genesisNodeViewTestInputs(
    genesisConsensusView:       NxtConsensus.Genesis
  )(implicit protocolVersioner: ProtocolVersioner): TestIn = {
    val historyComponents = generateHistory(genesisConsensusView.block)
    val stateComponents = generateState(genesisConsensusView.block)
    val nodeView = NodeView(
      historyComponents.history,
      stateComponents.state,
      MemPool.empty()
    )

    TestIn(
      nodeView,
      historyComponents.storage,
      stateComponents.tbrStore,
      stateComponents.pbrStore,
      genesisConsensusView
    )
  }

  def generateState(genesisBlock: Block)(implicit
    networkPrefix:                NetworkPrefix
  ): AccessibleState = {
    val tbrStore = InMemoryKeyValueStore.empty()
    val pbrStore = InMemoryKeyValueStore.empty()
    val stateStore = InMemoryKeyValueStore.empty()
    val tokenBoxRegistry = new TokenBoxRegistry(tbrStore)
    val programBoxRegistry = new ProgramBoxRegistry(pbrStore)
    val state = State(genesisBlock.id, stateStore, tokenBoxRegistry, programBoxRegistry)
    state.applyModifier(genesisBlock).get
    AccessibleState(state, stateStore, tbrStore, pbrStore)
  }

  def generateHistory(
    genesisBlock:           Block
  )(implicit networkPrefix: NetworkPrefix, protocolVersioner: ProtocolVersioner): AccessibleHistory = {
    val tineProcessor = TineProcessor(1024)
    val store = new InMemoryKeyValueStore()
    val storage = new Storage(store)
    val history = new History(storage, tineProcessor)
    history.append(genesisBlock, Seq()).get._1
    AccessibleHistory(history, store)
  }

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
        difficulty = leaderElection.calculateNewDifficulty(
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
