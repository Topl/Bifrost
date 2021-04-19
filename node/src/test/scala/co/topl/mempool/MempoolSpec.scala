package co.topl.mempool

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.util.Timeout
import co.topl.nodeView.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedTransaction}
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.nodeView.{CurrentView, NodeViewHolderRef}
import co.topl.settings.{AppContext, StartupOpts}
import co.topl.utils.CoreGenerators
import org.scalatest.{BeforeAndAfterAll, DoNotDiscover}
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext}

@DoNotDiscover
class MempoolSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with Matchers
  with CoreGenerators
  with BeforeAndAfterAll {

  private implicit val actorSystem: ActorSystem = ActorSystem(settings.network.agentName)
  private implicit val executionContext: ExecutionContext = actorSystem.dispatcher

  protected val appContext = new AppContext(settings, StartupOpts.empty, None)
  private val nodeViewHolderRef: ActorRef = NodeViewHolderRef("nodeViewHolder", settings, appContext)

  implicit val timeout: Timeout = Timeout(10.seconds)

  private def view() = Await.result(
    (nodeViewHolderRef ? GetDataFromCurrentView).mapTo[CurrentView[History, State, MemPool]],
    10.seconds)

  property("Repeated transactions already in history should be discarded " +
    "when received by the node view") {
    val txs = view().history.bestBlock.transactions
    txs.foreach(tx ⇒ nodeViewHolderRef ! LocallyGeneratedTransaction(tx))
    txs.foreach(tx ⇒ view().pool.contains(tx) shouldBe false)
  }

  override protected def afterAll(): Unit =
    Await.result(actorSystem.terminate(), 10.seconds)
}
