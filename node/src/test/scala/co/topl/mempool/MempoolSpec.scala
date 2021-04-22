package co.topl.mempool

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.util.Timeout
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction
import co.topl.network.NodeViewSynchronizer.ReceivableMessages.{ChangedHistory, ChangedMempool}
import co.topl.network.message.BifrostSyncInfo
import co.topl.nodeView.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, GetNodeViewChanges, LocallyGeneratedTransaction}
import co.topl.nodeView.history.{History, HistoryReader}
import co.topl.nodeView.mempool.{MemPool, MemPoolReader}
import co.topl.nodeView.state.State
import co.topl.nodeView.NodeViewHolderRef
import co.topl.settings.{AppContext, StartupOpts}
import co.topl.utils.CoreGenerators
import org.scalatest.DoNotDiscover
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext}

@DoNotDiscover
class MempoolSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers with CoreGenerators {

  private implicit val actorSystem: ActorSystem = ActorSystem(settings.network.agentName)
  private implicit val executionContext: ExecutionContext = actorSystem.dispatcher

  protected val appContext = new AppContext(settings, StartupOpts(), None)
  private val nodeViewHolderRef: ActorRef = NodeViewHolderRef("nodeViewHolder", settings, appContext)

  implicit val timeout: Timeout = Timeout(10.seconds)

  private def getHistory: HistoryReader[Block, BifrostSyncInfo] = Await.result(
    (nodeViewHolderRef ? GetNodeViewChanges(history = true, state = false, mempool = false))
      .mapTo[ChangedHistory[HistoryReader[Block, BifrostSyncInfo]]].map(_.reader),
    10.seconds
  )

  private def getMempool: MemPoolReader[Transaction.TX] = Await.result(
    (nodeViewHolderRef ? GetNodeViewChanges(history = false, state = false, mempool = true))
      .mapTo[ChangedMempool[MemPoolReader[Transaction.TX]]].map(_.reader),
    10.seconds
  )

  property(
    "Repeated transactions already in history should be discarded " +
    "when received by the node view"
  ) {
    val txs = getHistory.bestBlock.transactions
    txs.foreach(tx ⇒ nodeViewHolderRef ! LocallyGeneratedTransaction(tx))
    txs.foreach(tx ⇒ getMempool.contains(tx) shouldBe false)
  }
}
