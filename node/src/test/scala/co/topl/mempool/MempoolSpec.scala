package co.topl.mempool

import akka.actor.typed._
import akka.actor.typed.eventstream.EventStream
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter._
import akka.util.Timeout
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction
import co.topl.network.message.BifrostSyncInfo
import co.topl.nodeView.{LocallyGeneratedTransaction, NodeViewReaderWriter}
import co.topl.nodeView.history.HistoryReader
import co.topl.nodeView.mempool.MemPoolReader
import co.topl.settings.{AppContext, StartupOpts}
import co.topl.utils.{CommonGenerators, NodeGenerators}
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.{BeforeAndAfterAll, DoNotDiscover}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

@DoNotDiscover
class MempoolSpec
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with CommonGenerators
    with NodeGenerators
    with BeforeAndAfterAll {

  implicit private val actorSystem: ActorSystem[_] = ActorSystem(Behaviors.empty, settings.network.agentName)

  protected val appContext = new AppContext(settings, StartupOpts(), None)(actorSystem.toClassic)

  private val nodeViewHolderRef =
    actorSystem.systemActorOf(NodeViewReaderWriter(settings, appContext), NodeViewReaderWriter.ActorName)

  implicit val timeout: Timeout = Timeout(10.seconds)

  private def getHistory: HistoryReader[Block, BifrostSyncInfo] = Await.result(
    nodeViewHolderRef.ask[HistoryReader[Block, BifrostSyncInfo]](
      NodeViewReaderWriter.ReceivableMessages.Read(_.history, _)
    ),
    10.seconds
  )

  private def getMempool: MemPoolReader[Transaction.TX] = Await.result(
    nodeViewHolderRef.ask[MemPoolReader[Transaction.TX]](NodeViewReaderWriter.ReceivableMessages.Read(_.memPool, _)),
    10.seconds
  )

  property(
    "Repeated transactions already in history should be discarded " +
    "when received by the node view"
  ) {
    val txs = getHistory.bestBlock.transactions

    txs.foreach(tx => actorSystem.eventStream.tell(EventStream.Publish(LocallyGeneratedTransaction(tx))))
    txs.foreach(tx => getMempool.contains(tx) shouldBe false)
  }

  override protected def afterAll(): Unit =
    actorSystem.terminate()
}
