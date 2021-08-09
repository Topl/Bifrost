package co.topl.nodeView

import akka.actor.{Actor, ActorRef}
import akka.pattern.pipe
import akka.util.Timeout
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.CleanupWorker.{CleanupDecision, RunCleanup}
import co.topl.nodeView.MempoolAuditor.CleanupDone
import co.topl.settings.AppSettings
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.{Logging, TimeProvider}

import scala.collection.immutable.TreeSet
import scala.concurrent.Future

/**
 * Performs mempool validation task on demand.
 * Validation result is sent directly to `NodeViewHolder`.
 */
class CleanupWorker(
  nodeViewHolderRef: akka.actor.typed.ActorRef[NodeViewHolder.ReceivableMessage],
  settings:          AppSettings
)(implicit
  networkPrefix: NetworkPrefix,
  timeProvider:  TimeProvider
) extends Actor
    with Logging {

  implicit private val orderTransactions: Ordering[Transaction.TX] = Ordering.by(_.id)

  import context.dispatcher

  // keep some number of recently validated transactions in order
  // to avoid validating the same transactions too many times.
  private var validatedIndex: TreeSet[Transaction.TX] = TreeSet.empty[Transaction.TX]
  // count validation sessions in order to perform index cleanup.
  private var epochNr: Int = 0

  override def preStart(): Unit =
    log.info("Cleanup worker started")

  override def receive: Receive = {
    case RunCleanup =>
      val s = sender()
      withNodeView(splitIds(_, s))
        .pipeTo(context.self)

    case CleanupDecision(validatedTransactions, invalidatedTransactions, sender) =>
      sender ! CleanupDone(validatedTransactions.take(settings.application.rebroadcastCount))
      if (invalidatedTransactions.nonEmpty) {
        log.info(s"${invalidatedTransactions.size} transactions from mempool were invalidated")
        nodeViewHolderRef.tell(
          NodeViewHolder.ReceivableMessages.EliminateTransactions(invalidatedTransactions.map(_.id))
        )
      }
      if (epochNr < Int.MaxValue) epochNr += 1 else epochNr = 0
      if (epochNr % CleanupWorker.RevisionInterval == 0) {
        // drop old index in order to check potentially outdated transactions again.
        validatedIndex = TreeSet(validatedTransactions: _*)
      } else {
        validatedIndex ++= validatedTransactions
      }

    //Should not be here, if non-expected signal comes, check logic
    case a: Any => log.warn(s"Strange input: $a")
  }

  /**
   * Checks if the outputs of unconfirmed transactions exists in state or if the transaction has become
   *  stale (by exceeding the mempoolTimeout). If either are true, the transaction is removed from the mempool
   */
  private def splitIds(nodeView: ReadableNodeView, replyTo: ActorRef): CleanupDecision = {
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

    CleanupDecision(valid, invalid, replyTo)
  }

  private def withNodeView[T](f: ReadableNodeView => T): Future[T] = {
    import akka.actor.typed.scaladsl.AskPattern._
    import akka.actor.typed.scaladsl.adapter._

    import scala.concurrent.duration._
    implicit val timeout: Timeout = Timeout(10.seconds)
    implicit val typedSystem: akka.actor.typed.ActorSystem[_] = context.system.toTyped
    nodeViewHolderRef.askWithStatus[T](NodeViewHolder.ReceivableMessages.Read(f, _))
  }

}

object CleanupWorker {

  /**
   * Constant which shows on how many cleanup operations (called when a new block arrives) a transaction
   * re-check happens.
   *
   * If transactions set is large and stable, then about (1/RevisionInterval)-th of the pool is checked
   */
  val RevisionInterval: Int = 4

  case object RunCleanup

  private case class CleanupDecision(
    validatedTransactions:   Seq[Transaction.TX],
    invalidatedTransactions: Seq[Transaction.TX],
    replyTo:                 ActorRef
  )

}
