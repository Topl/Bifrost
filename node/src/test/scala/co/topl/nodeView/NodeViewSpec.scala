package co.topl.nodeView

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import co.topl.network.utils.NetworkTimeProvider
import co.topl.nodeView.mempool.MemPool
import co.topl.utils.{CommonGenerators, GenesisBlockGenerators, InMemoryKeyFileTestHelper, TestSettings, TimeProvider}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class NodeViewSpec
    extends AnyPropSpec
    with CommonGenerators
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with TestSettings
    with BeforeAndAfterAll
    with InMemoryKeyFileTestHelper
    with GenesisBlockGenerators {

  implicit private val actorSystem: ActorSystem[_] = ActorSystem(Behaviors.empty, settings.network.agentName)

  implicit private val timeProvidedr: TimeProvider = new NetworkTimeProvider(settings.ntp)

  property("Rewards transactions are removed from transactions extracted from a block being rolled back") {
    val genesis = genesisBlockGen(keyRing).pureApply(Gen.Parameters.default, Seed.random())
    val nodeView = NodeView.genesis(settings, appContext.networkType, genesis)
    forAll(blockGen) { block =>
      val polyReward = sampleUntilNonEmpty(polyTransferGen)
      val arbitReward = sampleUntilNonEmpty(arbitTransferGen)
      val rewardBlock = block.copy(transactions = Seq(arbitReward, polyReward), parentId = genesis.id)

      val memPool =
        nodeView.updateMemPool(List(rewardBlock), Nil, MemPool.emptyPool)

      memPool.contains(polyReward) shouldBe false
      memPool.contains(arbitReward) shouldBe false
    }
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    actorSystem.terminate()
  }
}
