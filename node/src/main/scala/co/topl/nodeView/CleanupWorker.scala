package co.topl.nodeView

import akka.actor.{Actor, ActorRef}
import akka.util.Timeout
import co.topl.modifier.ModifierId
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

  // keep some number of recently validated transactions in order
  // to avoid validating the same transactions too many times.
  private var validatedIndex: TreeSet[ModifierId] = TreeSet.empty[ModifierId]
  // count validation sessions in order to perform index cleanup.
  private var epochNr: Int = 0

  override def preStart(): Unit =
    log.info("Cleanup worker started")

  override def receive: Receive = {
    case RunCleanup =>
      import akka.pattern.pipe
      import context.dispatcher
      val s = sender()
      withNodeView(splitIds(_, s))
        .pipeTo(context.self)

    case CleanupDecision(validatedIds, idsToInvalidate, sender) =>
      sender ! CleanupDone(validatedIds.take(settings.application.rebroadcastCount))
      log.info(s"${idsToInvalidate.size} transactions from mempool were invalidated")
      nodeViewHolderRef.tell(NodeViewHolder.ReceivableMessages.EliminateTransactions(idsToInvalidate))
      if (epochNr < Int.MaxValue) epochNr += 1 else epochNr = 0
      if (epochNr % CleanupWorker.RevisionInterval == 0) {
        // drop old index in order to check potentially outdated transactions again.
        validatedIndex = TreeSet(validatedIds: _*)
      } else {
        validatedIndex ++= validatedIds
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
        .filterNot(utx => validatedIndex.contains(utx.tx.id))
        .foldLeft((Seq[ModifierId](), Seq[ModifierId]()))({
          case ((validAcc: Seq[ModifierId], invalidAcc: Seq[ModifierId]), utx) =>
            // if any newly created box matches a box already in the UTXO set, remove the transaction
            val boxAlreadyExists = utx.tx.newBoxes.exists(b => nodeView.state.getBox(b.id).isDefined)
            val txTimeout =
              (timeProvider.time - utx.dateAdded) > settings.application.mempoolTimeout.toMillis

            if (boxAlreadyExists | txTimeout) (validAcc, utx.tx.id +: invalidAcc)
            else (utx.tx.id +: validAcc, invalidAcc)
        })

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

  private case class CleanupDecision(validatedIds: Seq[ModifierId], idsToInvalidate: Seq[ModifierId], replyTo: ActorRef)

}
