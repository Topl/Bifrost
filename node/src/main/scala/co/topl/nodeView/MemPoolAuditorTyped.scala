package co.topl.nodeView

import akka.actor.typed.eventstream.EventStream
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.{ActorRef => CActorRef}
import akka.util.Timeout
import co.topl.modifier.transaction.Transaction
import co.topl.network.Broadcast
import co.topl.network.NetworkController.ReceivableMessages.SendToNetwork
import co.topl.network.message.{InvData, InvSpec, Message}
import co.topl.nodeView.MemPoolAuditorTyped.ReceivableMessage.RunCleanup
import co.topl.nodeView.NodeViewHolder.Events.SemanticallySuccessfulModifier
import co.topl.settings.AppSettings
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.TimeProvider
import org.slf4j.Logger

import scala.collection.immutable.TreeSet
import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * Controls mempool cleanup workflow. Watches NodeView events and delegates
 * mempool cleanup task to [[CleanupWorker]] when needed.
 * Adapted from ErgoPlatform available at https://github.com/ergoplatform/ergo
 */
object MemPoolAuditorTyped {

  val actorName = "mempoolAuditor"

  sealed abstract class ReceivableMessage

  object ReceivableMessage {

    private[nodeView] case class CleanupDone(toBeBroadcast: Seq[Transaction.TX]) extends ReceivableMessage

    private[nodeView] case object GotSemanticallySuccessfulModifier extends ReceivableMessage

    private[nodeView] case object RunCleanup extends ReceivableMessage

    private[nodeView] case class CleanupDecision(
      validatedTransactions:   Seq[Transaction.TX],
      invalidatedTransactions: Seq[Transaction.TX]
    ) extends ReceivableMessage

    private[nodeView] case class Fail(throwable: Throwable) extends ReceivableMessage

  }

  def apply(
    nodeViewHolderRef:      ActorRef[NodeViewHolder.ReceivableMessage],
    networkControllerRef:   CActorRef,
    settings:               AppSettings
  )(implicit networkPrefix: NetworkPrefix, timeProvider: TimeProvider): Behavior[ReceivableMessage] =
    Behaviors.setup { implicit context =>
      implicit val system: ActorSystem[_] = context.system
      implicit val orderTransactions: Ordering[Transaction.TX] = Ordering.by(_.id)

      system.eventStream.tell(
        EventStream.Subscribe[SemanticallySuccessfulModifier[_]](
          context.messageAdapter(_ => ReceivableMessage.GotSemanticallySuccessfulModifier)
        )
      )
      context.log.info(s"${Console.YELLOW}MemPool Auditor transitioning to the operational state${Console.RESET}")

      new MemPoolAuditorBehaviors(nodeViewHolderRef, networkControllerRef, settings)
        .idle(TreeSet.empty[Transaction.TX], 0)
    }
}

private class MemPoolAuditorBehaviors(
  nodeViewHolderRef:    ActorRef[NodeViewHolder.ReceivableMessage],
  networkControllerRef: CActorRef,
  settings:             AppSettings
)(implicit context:     ActorContext[MemPoolAuditorTyped.ReceivableMessage], timeProvider: TimeProvider) {
  import MemPoolAuditorTyped._
  implicit private val log: Logger = context.log
  implicit private val orderTransactions: Ordering[Transaction.TX] = Ordering.by(_.id)

  /**
   * Constant which shows on how many cleanup operations (called when a new block arrives) a transaction re-check
   * happens. If transactions set is large and stable, then about (1/RevisionInterval)-th of the pool is checked
   */
  val RevisionInterval: Int = 4

  def idle(validatedIndex: TreeSet[Transaction.TX], epochNr: Int): Behavior[ReceivableMessage] =
    Behaviors.receiveMessagePartial[ReceivableMessage] { case ReceivableMessage.GotSemanticallySuccessfulModifier =>
      context.self.tell(RunCleanup)
      active(validatedIndex, epochNr)
    }

  private def active(validatedIndex: TreeSet[Transaction.TX], epochNr: Int): Behavior[ReceivableMessage] =
    Behaviors.receiveMessagePartial[ReceivableMessage] {
      case ReceivableMessage.RunCleanup =>
        context.pipeToSelf(withNodeView(splitIds(validatedIndex, _))) {
          case Success(decision)  => decision
          case Failure(exception) => ReceivableMessage.Fail(exception)
        }
        Behaviors.same

      case ReceivableMessage.CleanupDecision(validatedTransactions, invalidatedTransactions) =>
        context.self.tell(
          ReceivableMessage.CleanupDone(validatedTransactions.take(settings.application.rebroadcastCount))
        )
        if (invalidatedTransactions.nonEmpty) {
          log.info(s"${invalidatedTransactions.size} transactions from mempool were invalidated")
          nodeViewHolderRef ! NodeViewHolder.ReceivableMessages.EliminateTransactions(invalidatedTransactions.map(_.id))
        }
        val newEpochNr = if (epochNr < Int.MaxValue) epochNr + 1 else 0
        if (epochNr % RevisionInterval == 0) {
          // drop old index in order to check potentially outdated transactions again.
          active(TreeSet(validatedTransactions: _*), newEpochNr)
        } else {
          active(validatedIndex ++ validatedTransactions, newEpochNr)
        }

      case ReceivableMessage.CleanupDone(ids) =>
        //TODO: Jing - remove color
        log.info(s"Cleanup done. Switching to idle mode")
        rebroadcastTransactions(ids)
        idle(validatedIndex, epochNr)

      case ReceivableMessage.Fail(throwable) =>
        throw throwable
    }

  /**
   * Checks if the outputs of unconfirmed transactions exists in state or if the transaction has become
   *  stale (by exceeding the mempoolTimeout). If either are true, the transaction is removed from the mempool
   */
  private def splitIds(
    validatedIndex:        TreeSet[Transaction.TX],
    nodeView:              ReadableNodeView
  )(implicit timeProvider: TimeProvider): ReceivableMessage.CleanupDecision = {
    val (valid, invalid) =
      nodeView.memPool
        .take(100)(-_.dateAdded)
        .filterNot(utx => validatedIndex.contains(utx.tx))
        .foldLeft((Seq[Transaction.TX](), Seq[Transaction.TX]())) { case ((validAcc, invalidAcc), utx) =>
          // if any newly created box matches a box already in the UTXO set, remove the transaction
          val boxAlreadyExists = utx.tx.newBoxes.exists(b => nodeView.state.getBox(b.id).isDefined)
          val txTimeout =
            (timeProvider.time - utx.dateAdded) > settings.application.mempoolTimeout.toMillis

          if (boxAlreadyExists || txTimeout) (validAcc, utx.tx +: invalidAcc)
          else (utx.tx +: validAcc, invalidAcc)
        }


    ReceivableMessage.CleanupDecision(valid, invalid)
  }

  private def withNodeView[T](f: ReadableNodeView => T): Future[T] = {
    import akka.actor.typed.scaladsl.AskPattern._

    import scala.concurrent.duration._
    implicit val timeout: Timeout = Timeout(10.seconds)
    implicit val typedSystem: ActorSystem[_] = context.system
    nodeViewHolderRef.askWithStatus[T](NodeViewHolder.ReceivableMessages.Read(f, _))
  }

  private def rebroadcastTransactions(transactions: Seq[Transaction.TX]): Unit =
    if (transactions.nonEmpty) {
      log.debug("Rebroadcasting transactions")
      transactions.foreach { tx =>
        log.info(s"Rebroadcasting $tx")
        val msg = Message(
          new InvSpec(settings.network.maxInvObjects),
          Right(InvData(Transaction.modifierTypeId, Seq(tx.id))),
          None
        )

        networkControllerRef ! SendToNetwork(msg, Broadcast)
      }
    } else {
      log.debug("No transactions to rebroadcast")
    }
}
