package co.topl.nodeView

import akka.actor.typed.eventstream.EventStream
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, SupervisorStrategy}
import akka.actor.{ActorRef => CActorRef}
import akka.util.Timeout
import co.topl.modifier.transaction.Transaction
import co.topl.network.Broadcast
import co.topl.network.NetworkController.ReceivableMessages.SendToNetwork
import co.topl.network.message.Messages.MessagesV1
import co.topl.network.message.Transmission
import co.topl.nodeView.NodeViewHolder.Events.SemanticallySuccessfulModifier
import co.topl.settings.AppSettings
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.TimeProvider
import org.slf4j.Logger

import scala.collection.immutable.TreeSet
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
 * Controls mempool cleanup workflow. Watches NodeView events and delegates
 * mempool cleanup task to [[CleanupWorker]] when needed.
 * Adapted from ErgoPlatform available at https://github.com/ergoplatform/ergo
 */
object MemPoolAuditor {

  val actorName = "mempoolAuditor"

  sealed abstract class ReceivableMessage

  object ReceivableMessages {

    private[nodeView] case class CleanupDone(toBeBroadcast: Seq[Transaction.TX]) extends ReceivableMessage

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
  )(implicit networkPrefix: NetworkPrefix, timeProvider: TimeProvider): Behavior[ReceivableMessage] = {
    val backoff =
      SupervisorStrategy.restartWithBackoff(minBackoff = 1.seconds, maxBackoff = 30.seconds, randomFactor = 0.1)

    Behaviors
      .supervise(Behaviors.setup[ReceivableMessage] { implicit context =>
        implicit val system: ActorSystem[_] = context.system
        implicit val orderTransactions: Ordering[Transaction.TX] = Ordering.by(_.id)

        system.eventStream.tell(
          EventStream.Subscribe[SemanticallySuccessfulModifier[_]](
            context.messageAdapter(_ => ReceivableMessages.RunCleanup)
          )
        )
        context.log.info(s"${Console.YELLOW}MemPool Auditor transitioning to the operational state${Console.RESET}")

        new MemPoolAuditorBehaviors(nodeViewHolderRef, networkControllerRef, settings)
          .idle(TreeSet.empty[Transaction.TX], 0)
      })
      .onFailure(backoff)
  }
}

private class MemPoolAuditorBehaviors(
  nodeViewHolderRef:    ActorRef[NodeViewHolder.ReceivableMessage],
  networkControllerRef: CActorRef,
  settings:             AppSettings
)(implicit context:     ActorContext[MemPoolAuditor.ReceivableMessage], timeProvider: TimeProvider) {
  import MemPoolAuditor._
  implicit private val log: Logger = context.log
  implicit private val orderTransactions: Ordering[Transaction.TX] = Ordering.by(_.id)

  /**
   * Constant which shows on how many cleanup operations (called when a new block arrives) a transaction re-check
   * happens. If transactions set is large and stable, then about (1/RevisionInterval)-th of the pool is checked
   */
  val RevisionInterval: Int = 4

  def idle(validatedTx: TreeSet[Transaction.TX], iteration: Int): Behavior[ReceivableMessage] =
    Behaviors.receiveMessagePartial[ReceivableMessage] { case ReceivableMessages.RunCleanup =>
      context.pipeToSelf(withNodeView(splitIds(validatedTx, _))) {
        case Success(decision)  => decision
        case Failure(exception) => ReceivableMessages.Fail(exception)
      }
      awaitingDecision(validatedTx, iteration)
    }

  private def awaitingDecision(validatedTx: TreeSet[Transaction.TX], iteration: Int): Behavior[ReceivableMessage] =
    Behaviors.receiveMessagePartial[ReceivableMessage] {

      case ReceivableMessages.CleanupDecision(validatedTransactions, invalidatedTransactions) =>
        if (invalidatedTransactions.nonEmpty) {
          log.info(s"${invalidatedTransactions.size} transactions from mempool were invalidated")
          nodeViewHolderRef ! NodeViewHolder.ReceivableMessages.EliminateTransactions(invalidatedTransactions.map(_.id))
        }
        if (validatedTransactions.nonEmpty) {
          rebroadcastTransactions(validatedTransactions.take(settings.application.rebroadcastCount))
        }

        log.info(s"Cleanup done. Switching to idle mode")

        val newIteration = if (iteration < Int.MaxValue) iteration + 1 else 0
        if (newIteration % RevisionInterval == 0) {
          // drop old index in order to check potentially outdated transactions again.
          idle(TreeSet.empty ++ validatedTransactions, newIteration)
        } else {
          idle(validatedTx ++ validatedTransactions, newIteration)
        }

      case ReceivableMessages.Fail(throwable) =>
        throw throwable
    }

  /**
   * Checks if the outputs of unconfirmed transactions exists in state or if the transaction has become
   *  stale (by exceeding the mempoolTimeout). If either are true, the transaction is removed from the mempool
   */
  private def splitIds(
    validatedIndex:        TreeSet[Transaction.TX],
    nodeView:              ReadableNodeView
  )(implicit timeProvider: TimeProvider): ReceivableMessages.CleanupDecision = {
    val (valid, invalid) =
      nodeView.memPool
        .take(Int.MaxValue)(-_.dateAdded)
        .filterNot(utx => validatedIndex.contains(utx.tx))
        .foldLeft((Seq[Transaction.TX](), Seq[Transaction.TX]())) { case ((validAcc, invalidAcc), utx) =>
          // if any newly created box matches a box already in the UTXO set, remove the transaction
          lazy val newBoxAlreadyExists = utx.tx.newBoxes.exists(b => nodeView.state.getBox(b.id).isDefined)
          lazy val inputBoxAlreadyUsed = utx.tx.boxIdsToOpen.exists(id => nodeView.state.getBox(id).isEmpty)
          val txTimeout =
            (timeProvider.time - utx.dateAdded) > settings.application.mempoolTimeout.toMillis

          if (txTimeout || newBoxAlreadyExists || inputBoxAlreadyUsed) (validAcc, utx.tx +: invalidAcc)
          else (utx.tx +: validAcc, invalidAcc)
        }

    ReceivableMessages.CleanupDecision(valid, invalid)
  }

  private def withNodeView[T](f: ReadableNodeView => T): Future[T] = {
    import akka.actor.typed.scaladsl.AskPattern._

    import scala.concurrent.duration._
    implicit val timeout: Timeout = Timeout(10.seconds)
    implicit val typedSystem: ActorSystem[_] = context.system
    nodeViewHolderRef.askWithStatus[T](NodeViewHolder.ReceivableMessages.Read(f, _))
  }

  // TODO: Jing - Consider changing this to just sending the ids to the networkController and let it form the message
  private def rebroadcastTransactions(transactions: Seq[Transaction.TX]): Unit = {
    log.debug("Rebroadcasting transactions")

    val msg = MessagesV1.InventoryResponse(Transaction.modifierTypeId, transactions.map(_.id))
    networkControllerRef ! SendToNetwork(Transmission.encodeMessage(msg), msg.version, Broadcast)
  }
}
