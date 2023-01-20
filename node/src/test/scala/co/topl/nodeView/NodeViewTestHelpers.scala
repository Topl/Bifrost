package co.topl.nodeView

import co.topl.consensus._
import co.topl.modifier.block.Block
import co.topl.nodeView.NodeViewTestHelpers.TestIn
import co.topl.nodeView.history.{History, InMemoryKeyValueStore, Storage, TineProcessor}
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.{BoxState, ProgramBoxRegistry, TokenBoxRegistry}
import co.topl.utils.InMemoryKeyRingTestHelper
import co.topl.utils.NetworkType.NetworkPrefix
import org.scalatest.{BeforeAndAfterAll, Suite}

trait NodeViewTestHelpers extends BeforeAndAfterAll with InMemoryKeyRingTestHelper with ValidBlockchainGenerator {
  self: Suite =>

  def nodeViewValidChainTestInputs(
    lengthOfChain: Byte
  )(implicit protocolVersioner: ProtocolVersioner): TestIn = {
    val balancePerParticipant = Int.MaxValue
    val initialDifficulty = Long.MaxValue

    val totalStake = keyRingCurve25519.addresses.size * balancePerParticipant

    val genesisHeadChain = validChainFromGenesis(
      keyRingCurve25519,
      balancePerParticipant,
      initialDifficulty,
      protocolVersioner
    )(lengthOfChain).sample.get

    val historyComponents = generateHistory(genesisHeadChain.head)
    val appenedHistory = historyComponents.copy(
      history = historyComponents.history match {
        case h: History =>
          genesisHeadChain.tail.foldLeft(h)((accHistory, block) =>
            accHistory.append(block, Seq(), genesisHeadChain.head.state).get._1
          )
      }
    )

    val stateComponents = generateState(genesisHeadChain.head.block)
    val appendedState = stateComponents.copy(
      state = stateComponents.state match {
        case h: BoxState => genesisHeadChain.tail.foldLeft(h)((accState, block) => accState.applyModifier(block).get)
      }
    )

    val nodeView = NodeView(
      appenedHistory.history,
      appendedState.state,
      MemPool.empty()
    )

    TestIn(
      nodeView,
      appenedHistory.storage,
      appendedState.tbrStore,
      appendedState.pbrStore,
      NxtConsensus.Genesis(
        genesisHeadChain.head.block,
        NxtConsensus.State(totalStake, 0L)
      )
    )

  }

  def nodeViewGenesisOnlyTestInputs(
    genesis: NxtConsensus.Genesis
  )(implicit protocolVersioner: ProtocolVersioner): TestIn = {
    val historyComponents = generateHistory(genesis)
    val stateComponents = generateState(genesis.block)
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
      genesis
    )
  }

  def generateState(genesisBlock: Block)(implicit
    networkPrefix: NetworkPrefix
  ): NodeViewTestHelpers.AccessibleState = {
    val tbrStore = InMemoryKeyValueStore.empty
    val pbrStore = InMemoryKeyValueStore.empty
    val stateStore = InMemoryKeyValueStore.empty
    val tokenBoxRegistry = new TokenBoxRegistry(tbrStore)
    val programBoxRegistry = new ProgramBoxRegistry(pbrStore)
    val state = BoxState(genesisBlock.id, stateStore, tokenBoxRegistry, programBoxRegistry)
    state.applyModifier(genesisBlock).get
    NodeViewTestHelpers.AccessibleState(state, stateStore, tbrStore, pbrStore)
  }

  def generateHistory(
    genesis: NxtConsensus.Genesis
  )(implicit
    networkPrefix:     NetworkPrefix,
    protocolVersioner: ProtocolVersioner
  ): NodeViewTestHelpers.AccessibleHistory = {
    val tineProcessor = TineProcessor(1024)
    val store = new InMemoryKeyValueStore()
    val storage = new Storage(store)
    val history = new History(storage, tineProcessor)
    history.append(genesis.block, Seq(), genesis.state).get._1
    NodeViewTestHelpers.AccessibleHistory(history, store)
  }
}

object NodeViewTestHelpers {

  case class TestIn(
    nodeView:      NodeView,
    historyStore:  InMemoryKeyValueStore,
    stateStore:    InMemoryKeyValueStore,
    tokenBoxStore: InMemoryKeyValueStore,
    genesis:       NxtConsensus.Genesis
  )

  case class AccessibleHistory(history: History, storage: InMemoryKeyValueStore)

  case class AccessibleState(
    state:      BoxState,
    stateStore: InMemoryKeyValueStore,
    tbrStore:   InMemoryKeyValueStore,
    pbrStore:   InMemoryKeyValueStore
  )
}
