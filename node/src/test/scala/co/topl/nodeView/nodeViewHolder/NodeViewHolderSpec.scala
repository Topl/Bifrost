package co.topl.nodeView.nodeViewHolder

import akka.actor.ActorSystem
import akka.testkit.TestActorRef
import co.topl.nodeView.NodeViewHolder
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.MockState
import co.topl.settings.{AppContext, StartupOpts}
import co.topl.utils.CoreGenerators
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.{BeforeAndAfterAll, PrivateMethodTester}

import scala.concurrent.ExecutionContext

class NodeViewHolderSpec extends AnyPropSpec
  with PrivateMethodTester
  with CoreGenerators
  with MockState
  with BeforeAndAfterAll {

  type MP = MemPool

  private implicit val actorSystem: ActorSystem = ActorSystem("NodeviewHolderSpec")
  private implicit val executionContext: ExecutionContext = actorSystem.dispatcher

  private val appContext = new AppContext(settings, StartupOpts(), None)
  private val nvhTestRef = TestActorRef(new NodeViewHolder(settings, appContext), "nvhTest")
  private val nodeView = nvhTestRef.underlyingActor
  private val state = createState()

  // TODO replace reward transactions with valid transactions
  property("Rewards transactions are removed from transactions extracted from a block being rolled back") {
    forAll(blockGen) { block =>
      val polyReward = sampleUntilNonEmpty(polyTransferGen)
      val arbitReward = sampleUntilNonEmpty(arbitTransferGen)
      val rewardBlock = block.copy(transactions = Seq(arbitReward, polyReward))

      val updateMemPool = PrivateMethod[MP]('updateMemPool)
      val memPool = nodeView invokePrivate updateMemPool(Seq(rewardBlock), Seq(), MemPool.emptyPool, state)
      memPool.contains(polyReward) shouldBe false
      memPool.contains(arbitReward) shouldBe false
    }
  }

  override protected def afterAll(): Unit = {
    actorSystem.terminate()
  }
}
