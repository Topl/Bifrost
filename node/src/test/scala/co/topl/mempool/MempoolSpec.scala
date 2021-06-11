package co.topl.mempool

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.util.Timeout
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction
import co.topl.network.NodeViewSynchronizer.ReceivableMessages.{ChangedHistory, ChangedMempool}
import co.topl.network.message.BifrostSyncInfo
import co.topl.nodeView.NodeViewHolder.ReceivableMessages.{GetNodeViewChanges, LocallyGeneratedTransaction}
import co.topl.nodeView.NodeViewHolderRef
import co.topl.nodeView.history.HistoryReader
import co.topl.nodeView.mempool.MemPoolReader
import co.topl.settings.{AppContext, StartupOpts}
import co.topl.utils.NodeGenerators
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.{BeforeAndAfterAll, DoNotDiscover}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext}

@DoNotDiscover
class MempoolSpec extends AnyPropSpec with Matchers with NodeGenerators with BeforeAndAfterAll {

  implicit private val actorSystem: ActorSystem = ActorSystem(settings.network.agentName)
  implicit private val executionContext: ExecutionContext = actorSystem.dispatcher

  protected val appContext = new AppContext(settings, StartupOpts(), None)
  private val nodeViewHolderRef: ActorRef = NodeViewHolderRef("nodeViewHolder", settings, appContext)

  implicit val timeout: Timeout = Timeout(10.seconds)

  private def getHistory: HistoryReader[Block, BifrostSyncInfo] = Await.result(
    (nodeViewHolderRef ? GetNodeViewChanges(history = true, state = false, mempool = false))
      .mapTo[ChangedHistory[HistoryReader[Block, BifrostSyncInfo]]]
      .map(_.reader),
    10.seconds
  )

  private def getMempool: MemPoolReader[Transaction.TX] = Await.result(
    (nodeViewHolderRef ? GetNodeViewChanges(history = false, state = false, mempool = true))
      .mapTo[ChangedMempool[MemPoolReader[Transaction.TX]]]
      .map(_.reader),
    10.seconds
  )

  property(
    "Repeated transactions already in history should be discarded " +
    "when received by the node view"
  ) {
    val txs = getHistory.bestBlock.transactions
    txs.foreach(tx => nodeViewHolderRef ! LocallyGeneratedTransaction(tx))
    txs.foreach(tx => getMempool.contains(tx) shouldBe false)
  }

  override protected def afterAll(): Unit =
    Await.result(actorSystem.terminate(), 10.seconds)
}
