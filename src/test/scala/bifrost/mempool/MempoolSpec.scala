package bifrost.mempool

import akka.actor.ActorRef
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.pattern.ask
import akka.util.Timeout
import bifrost.BifrostGenerators
import bifrost.history.History
import bifrost.modifier.transaction.bifrostTransaction.Transaction
import bifrost.nodeView.GenericNodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedTransaction}
import bifrost.nodeView.{CurrentView, NodeViewHolderRef}
import bifrost.settings.BifrostContext
import bifrost.state.State
import bifrost.wallet.Wallet
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

class MempoolSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with Matchers
  with ScalatestRouteTest
  with BifrostGenerators {

  protected val bifrostContext = new BifrostContext(settings, None)
  private val nodeViewHolderRef: ActorRef = NodeViewHolderRef("nodeViewHolder", settings, bifrostContext)

  implicit val timeout: Timeout = Timeout(10.seconds)

  private def actOnCurrentView(v: CurrentView[History, State, Wallet, MemPool]): CurrentView[History, State, Wallet, MemPool] = v

  private def view() = Await.result(
    (nodeViewHolderRef ? GetDataFromCurrentView(actOnCurrentView)).mapTo[CurrentView[History, State, Wallet, MemPool]],
    10.seconds)

  property("Repeated transactions already in history should be discarded " +
    "when received by the node view") {
    val txs = view().history.bestBlock.txs
    txs.foreach(tx ⇒ nodeViewHolderRef ! LocallyGeneratedTransaction[Transaction](tx))
    txs.foreach(tx ⇒ view().pool.contains(tx) shouldBe false)
  }
}
