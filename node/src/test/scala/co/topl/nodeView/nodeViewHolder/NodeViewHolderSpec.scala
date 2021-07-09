package co.topl.nodeView.nodeViewHolder

import akka.actor.typed._
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter._
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.{MockState, State}
import co.topl.nodeView.{NodeViewHolder, NodeViewWriter}
import co.topl.settings.{AppContext, StartupOpts}
import co.topl.utils.CommonGenerators
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.{BeforeAndAfterAll, PrivateMethodTester}

class NodeViewHolderSpec
    extends AnyPropSpec
    with PrivateMethodTester
    with CommonGenerators
    with MockState
    with BeforeAndAfterAll {

  type MP = MemPool

  implicit private val actorSystem: ActorSystem[_] = ActorSystem(Behaviors.empty, settings.network.agentName)

  private var appContext: AppContext = _

  private val nodeViewHolderRef =
    actorSystem.systemActorOf(NodeViewHolder(settings, appContext), NodeViewHolder.ActorName)

  private var state: State = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    appContext = new AppContext(settings, StartupOpts(), None)(actorSystem.toClassic)
    state = createState(inTempFile = false)
  }

  // TODO replace reward transactions with valid transactions
  property("Rewards transactions are removed from transactions extracted from a block being rolled back") {
    forAll(blockGen) { block =>
      val polyReward = sampleUntilNonEmpty(polyTransferGen)
      val arbitReward = sampleUntilNonEmpty(arbitTransferGen)
      val rewardBlock = block.copy(transactions = Seq(arbitReward, polyReward))

      val writer = new NodeViewWriter(appContext)

      val memPool =
        writer.updateMemPool(List(rewardBlock), Nil, MemPool.emptyPool)

      memPool.contains(polyReward) shouldBe false
      memPool.contains(arbitReward) shouldBe false
    }
  }

  override protected def afterAll(): Unit =
    actorSystem.terminate()
}
