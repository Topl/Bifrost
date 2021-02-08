package co.topl.nodeView

import akka.actor.{Actor, ActorRef}
import co.topl.attestation.Address
import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.modifier.ModifierId
import co.topl.modifier.box.{BoxId, ProgramId}
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.CleanupWorker.RunCleanup
import co.topl.nodeView.MempoolAuditor.CleanupDone
import co.topl.nodeView.NodeViewHolder.ReceivableMessages.EliminateTransactions
import co.topl.nodeView.mempool.MemPoolReader
import co.topl.nodeView.state.StateReader
import co.topl.settings.AppSettings
import co.topl.utils.Logging

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import scala.reflect.ClassTag
import scala.util.{Failure, Success}

/**
 * Performs mempool validation task on demand.
 * Validation result is sent directly to `NodeViewHolder`.
 */
class CleanupWorker[
  TX <: Transaction.TX,
  SR <: StateReader[ProgramId, Address]: ClassTag,
  MR <: MemPoolReader[TX]: ClassTag
](nodeViewHolderRef: ActorRef,
  settings: AppSettings)(implicit networkPrefix: NetworkPrefix) extends Actor with Logging {

  // keep some number of recently validated transactions in order
  // to avoid validating the same transactions too many times.
  private var validatedIndex: TreeSet[ModifierId] = TreeSet.empty[ModifierId]
  // count validation sessions in order to perform index cleanup.
  private var epochNr: Int = 0

  override def preStart(): Unit = {
    log.info("Cleanup worker started")
  }

  override def receive: Receive = {
    case RunCleanup(state, mempool) =>
      runCleanup(state, mempool)
      sender() ! CleanupDone

    //Should not be here, if non-expected signal comes, check logic
    case a: Any => log.warn(s"Strange input: $a")
  }

  private def runCleanup[S <: SR, M <: MR](state: S, mempool: M): Unit = {
    val toEliminate = validatePool(state, mempool)
    if (toEliminate.nonEmpty) {
      log.info(s"${toEliminate.size} transactions from mempool were invalidated")
      nodeViewHolderRef ! EliminateTransactions(toEliminate)
    }
  }

  /**
   * Validates transactions from mempool for some specified amount of time.
   *
   * @return - invalidated transaction ids
   */
  private def validatePool(stateReader: SR, mempool: MR): Seq[ModifierId] = {


    // Check transactions sorted by priority. Parent transaction comes before its children.
    val txsToValidate = mempool.take(100)

    //internal loop function validating transactions, returns validated and invalidated transaction ids
    @tailrec
    def validationLoop(txs: Seq[TX],
                       validated: Seq[ModifierId],
                       invalidated: Seq[ModifierId],
                       etAcc: Long): (Seq[ModifierId], Seq[ModifierId]) = {
      txs match {
        case head :: tail if etAcc < settings.application.mempoolCleanupDuration.toNanos && !validatedIndex.contains(head.id) =>

          // TODO: JAA - This should take into account previously validated transactions from the pool.
          val t0 = System.nanoTime()
          val validationResult = head.semanticValidate(stateReader)
          val t1 = System.nanoTime()
          val accumulatedTime = etAcc + (t1 - t0)

          val txId = head.id
          validationResult match {
            case Success(_) =>
              validationLoop(tail, validated :+ txId, invalidated, accumulatedTime)
            case Failure(e) =>
              log.info(s"Transaction $txId invalidated: ${e.getMessage}")
              validationLoop(tail, validated, invalidated :+ txId, accumulatedTime)
          }
        case _ :: tail if etAcc < settings.mempoolCleanupDuration.toNanos =>
          // this transaction was validated earlier, skip it
          validationLoop(tail, validated, invalidated, etAcc)
        case _ =>
          validated -> invalidated
      }
    }

    val (validatedIds, invalidatedIds) = validationLoop(txsToValidate, Seq.empty, Seq.empty, 0L)

    epochNr += 1
    if (epochNr % CleanupWorker.RevisionInterval == 0) {
      // drop old index in order to check potentially outdated transactions again.
      validatedIndex = TreeSet(validatedIds: _*)
    } else {
      validatedIndex ++= validatedIds
    }

    invalidatedIds
  }

}

object CleanupWorker {

  /**
   * Constant which shows on how many cleanup operations (called when a new block arrives) a transaction
   * re-check happens.
   *
   * If transactions set is large and stable, then about (1/RevisionInterval)-th of the pool is checked
   *
   */
  val RevisionInterval: Int = 4

  /**
   *
   * A command to run (partial) memory pool cleanup
   *
   * @param validator - a state implementation which provides transaction validation
   * @param mempool - mempool reader instance
   */
  case class RunCleanup(stateReader: StateReader[ProgramId, Address], mempool: MemPoolReader[Transaction.TX])

}
