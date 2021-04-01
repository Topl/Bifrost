package co.topl.nodeView.nodeViewHolder

import akka.testkit.TestActorRef
import co.topl.nodeView.NodeViewHolder
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.MockState
import co.topl.utils.CoreGenerators
import org.scalatest.PrivateMethodTester
import org.scalatest.propspec.AnyPropSpec

class NodeViewHolderSpec extends AnyPropSpec
  with PrivateMethodTester
  with CoreGenerators
  with MockState {

  type MP = MemPool

  private val nvhTestRef = TestActorRef(new NodeViewHolder(settings, appContext))
  private val nodeView = nvhTestRef.underlyingActor
  private val state = createState()

  // TODO replace reward transactions with valid transactions
  property("Rewards transactions are removed from transactions extracted from a block being rolled back") {
    forAll(blockGen) { block =>
      val polyReward = polyTransferGen.sample.get.copy(minting = true)
      val arbitReward = arbitTransferGen.sample.get.copy(minting = true)
      val rewardBlock = block.copy(transactions = Seq(arbitReward, polyReward))

      val updateMemPool = PrivateMethod[MP]('updateMemPool)
      val memPool = nodeView invokePrivate updateMemPool(Seq(rewardBlock), Seq(), MemPool.emptyPool, state)
      memPool.size shouldBe 0
    }
  }
}
