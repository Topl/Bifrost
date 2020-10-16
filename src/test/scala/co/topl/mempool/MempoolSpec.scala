package co.topl.mempool

import akka.actor.ActorRef
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.pattern.ask
import akka.util.Timeout
import co.topl.BifrostGenerators
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.GenericNodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedTransaction}
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.nodeView.{CurrentView, NodeViewHolderRef}
import co.topl.settings.AppContext
import co.topl.wallet.WalletConnectionHandler
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

  protected val appContext = new AppContext(settings, None)
  private val nodeViewHolderRef: ActorRef = NodeViewHolderRef("nodeViewHolder", settings, appContext)

  implicit val timeout: Timeout = Timeout(10.seconds)

  private def view() = Await.result(
    (nodeViewHolderRef ? GetDataFromCurrentView).mapTo[CurrentView[History, State, MemPool]],
    10.seconds)

  ignore("Repeated transactions already in history should be discarded " +
    "when received by the node view") {
    val txs = view().history.bestBlock.transactions
    txs.foreach(tx ⇒ nodeViewHolderRef ! LocallyGeneratedTransaction[Transaction](tx))
    txs.foreach(tx ⇒ view().pool.contains(tx) shouldBe false)
  }
}
