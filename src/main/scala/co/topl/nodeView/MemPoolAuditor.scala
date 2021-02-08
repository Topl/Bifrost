package co.topl.nodeView

import akka.actor.SupervisorStrategy.{Restart, Stop}
import akka.actor.{Actor, ActorInitializationException, ActorKilledException, ActorRef, ActorRefFactory, DeathPactException, OneForOneStrategy, Props}
import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.modifier.block.{Block, BlockHeader}
import co.topl.modifier.box.{BoxId, ProgramId}
import co.topl.modifier.transaction.Transaction
import co.topl.network.Broadcast
import co.topl.network.NetworkController.ReceivableMessages.SendToNetwork
import co.topl.network.NodeViewSynchronizer.ReceivableMessages.{ChangedMempool, ChangedState, SemanticallySuccessfulModifier}
import co.topl.network.message.{InvData, InvSpec, Message}
import co.topl.nodeView.CleanupWorker.RunCleanup
import co.topl.nodeView.MempoolAuditor.CleanupDone
import co.topl.nodeView.NodeViewHolder.ReceivableMessages.GetNodeViewChanges
import co.topl.nodeView.mempool.MemPoolReader
import co.topl.nodeView.state.StateReader
import co.topl.settings.AppSettings
import co.topl.utils.Logging

import scala.reflect.ClassTag

// need to add comments, node ready commands, name of actor in object,

import scala.concurrent.duration._

/** Controls mempool cleanup workflow. Watches NodeView events and delegates
  * mempool cleanup task to [[CleanupWorker]] when needed.
  * Adapted from ErgoPlatform available at https://github.com/ergoplatform/ergo
  */
class MempoolAuditor[
  SR <: StateReader[ProgramId, BoxId]: ClassTag,
  MR <: MemPoolReader[Transaction.TX]: ClassTag
](nodeViewHolderRef: ActorRef, networkControllerRef: ActorRef, settings: AppSettings)(implicit
  networkPrefix:     NetworkPrefix
) extends Actor
    with Logging {

  override def postRestart(reason: Throwable): Unit = {
    log.error(s"Mempool auditor actor restarted due to ${reason.getMessage}", reason)
    super.postRestart(reason)
  }

  override def postStop(): Unit = {
    logger.info("Mempool auditor stopped")
    super.postStop()
  }

  override val supervisorStrategy: OneForOneStrategy =
    OneForOneStrategy(maxNrOfRetries = 5, withinTimeRange = 1.minute) {
      case _: ActorKilledException => Stop
      case _: DeathPactException   => Stop
      case e: ActorInitializationException =>
        log.warn(s"Cleanup worker failed during initialization with: $e")
        Stop
      case e: Exception =>
        log.warn(s"Cleanup worker failed with: $e")
        context become awaiting // turn ctx into awaiting mode if worker failed
        Restart
    }

  private var stateReaderOpt: Option[SR] = None
  private var poolReaderOpt: Option[MR] = None

  private val worker: ActorRef =
    context.actorOf(Props(new CleanupWorker(nodeViewHolderRef, settings)))

  override def preStart(): Unit =
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])

  override def receive: Receive = awaiting

  private def awaiting: Receive = {
    case SemanticallySuccessfulModifier(_: Block) | SemanticallySuccessfulModifier(_: BlockHeader) =>
      stateReaderOpt = None
      poolReaderOpt = None
      nodeViewHolderRef ! GetNodeViewChanges(history = false, state = true, mempool = true)

    case ChangedMempool(mp: MR) =>
      poolReaderOpt = Some(mp)
      stateReaderOpt.foreach(st => initiateCleanup(st, mp))

    case ChangedState(st: SR) =>
      stateReaderOpt = Some(st)
      poolReaderOpt.foreach(mp => initiateCleanup(st, mp))

    case ChangedState(_) | ChangedMempool(_) => // do nothing
  }

  private def working: Receive = {
    case CleanupDone =>
      log.info("Cleanup done. Switching to awaiting mode")
      rebroadcastTransactions()
      stateReaderOpt = None
      poolReaderOpt = None
      context become awaiting

    case _ => // ignore other triggers until work is done
  }

  private def initiateCleanup(state: SR, mempool: MR): Unit = {
    log.info("Initiating cleanup. Switching to working mode")
    worker ! RunCleanup(state, mempool)
    context become working // ignore other triggers until work is done
  }

  private def rebroadcastTransactions(): Unit = {
    log.debug("Rebroadcasting transactions")
    poolReaderOpt.foreach { pr =>
      pr.take(settings.application.rebroadcastCount).foreach { tx =>
        log.info(s"Rebroadcasting $tx")
        val msg = Message(
          new InvSpec(settings.network.maxInvObjects),
          Right(InvData(Transaction.modifierTypeId, Seq(tx.id))),
          None
        )
        networkControllerRef ! SendToNetwork(msg, Broadcast)
      }

    }

  }
}

object MempoolAuditor {

  case object CleanupDone

}

object MempoolAuditorRef {

  def props(nodeViewHolderRef: ActorRef, networkControllerRef: ActorRef, settings: AppSettings)(implicit
    networkPrefix:             NetworkPrefix
  ): Props =
    Props(new MempoolAuditor(nodeViewHolderRef, networkControllerRef, settings))

  def apply(nodeViewHolderRef: ActorRef, networkControllerRef: ActorRef, settings: AppSettings)(implicit
    context:                   ActorRefFactory,
    networkPrefix:             NetworkPrefix
  ): ActorRef =
    context.actorOf(props(nodeViewHolderRef, networkControllerRef, settings))

}
