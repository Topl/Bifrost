package co.topl.consensus

import akka.actor._
import co.topl.crypto.PrivateKey25519
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.{ CoinbaseTransaction, Transaction }
import co.topl.network.message.BifrostSyncInfo
import co.topl.nodeView.CurrentView
import co.topl.nodeView.box.ArbitBox
import co.topl.nodeView.box.proposition.PublicKey25519Proposition
import co.topl.nodeView.history.{ History, HistoryReader }
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.settings.{ AppContext, ForgingSettings }
import co.topl.utils.Logging
import co.topl.wallet.Wallet

import scala.concurrent.ExecutionContext
import scala.util.{ Failure, Success, Try }

/**
 * Forger takes care of attempting to create new blocks using the wallet provided in the NodeView
 * Must be singleton
 */
class Forger(viewHolderRef: ActorRef, settings: ForgingSettings, appContext: AppContext)
            (implicit ec: ExecutionContext) extends Actor with Logging {

  type HR = HistoryReader[Block, BifrostSyncInfo]
  type CV = CurrentView[History, State, Wallet, MemPool]

  // Import the types of messages this actor RECEIVES
  import Forger.ReceivableMessages._

  // Import the types of messages this actor SENDS
  import co.topl.nodeView.GenericNodeViewHolder.ReceivableMessages.{ GetDataFromCurrentView, LocallyGeneratedModifier }

  private val keyFileDir = settings.keyFileDir.ensuring(_.isDefined, "A keyfile directory must be specified").get
  private val keyRing = KeyRing(keyFileDir)

  val TransactionsInBlock = 100 //should be a part of consensus, but for our app is okay

  override def preStart(): Unit = {
    targetBlockTime = settings.targetBlockTime

    if (settings.tryForging) {
      context.system.scheduler.scheduleOnce(settings.blockGenerationDelay)(self ! StartForging)
      context become readyToForge
    }
  }

////////////////////////////////////////////////////////////////////////////////////
////////////////////////////// ACTOR MESSAGE HANDLING //////////////////////////////

  // ----------- CONTEXT && MESSAGE PROCESSING FUNCTIONS
  override def receive: Receive = {
    case StartForging =>
      log.info(s"Forger: Received a START signal while forging disabled")

    case _ => nonsense
  }

  private def readyToForge: Receive = {
    case StartForging =>
      log.info("No Better Neighbor. Forger starts forging now.")
      viewHolderRef ! GetDataFromCurrentView(actOnCurrentView)
      context become activeForging

    case StopForging =>
      log.warn(s"Forger: Received a STOP signal while not forging. Signal ignored")

    case _ => nonsense
  }

  private def activeForging: Receive = {
    case StartForging =>
      log.warn(s"Forger: Received a START signal while forging. Signal ignored")

    case StopForging =>
      log.info(s"Forger: Received a stop signal. Forging will terminate after this trial")
      context become readyToForge

    case CurrentView(h: History, s: State, w: Wallet, m: MemPool) =>
      tryForging(h, s, w, m)

    case _ => nonsense
  }

  private def nonsense: Receive = {
    case nonsense: Any =>
      log.warn(s"Forger (in context ${context.toString}): got unexpected input $nonsense from ${sender()}")
  }

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// METHOD DEFINITIONS ////////////////////////////////
  /**
   * wrapper function to encapsulate the returned CurrentView
   *
   * @param view the view returned from NodeViewHolder
   * @return
   */
  def actOnCurrentView(view: CV): CV = view

  private def tryForging(h: HR, s: State, w: Wallet, m: MemPool): Unit = {
    log.info(s"${Console.CYAN}Trying to generate a new block, chain length: ${h.height}${Console.RESET}")
    log.info("chain difficulty: " + h.difficulty)

    val boxes: Seq[ArbitBox] = w.boxes().filter(_.box match {
      case a: ArbitBox => s.getBox(a.id).isDefined
      case _ => false
    }).map(_.box.asInstanceOf[ArbitBox])

    val boxKeys = boxes.flatMap(b => w.secretByPublicImage(b.proposition).map(s => (b, s)))
    log.debug(s"Trying to generate block on top of ${h.bestBlock.id} with balance " +
      s"${boxKeys.map(_._1.value).sum}")

    val transactions = pickTransactions(m, s, w, h.bestBlock).get

    iteration(h.bestBlock, h.difficulty, boxKeys, transactions, settings.version) match {
      case Some(block) =>
        log.debug(s"Locally generated block: $block")
        viewHolderRef ! LocallyGeneratedModifier[Block](block)
      case None =>
        log.debug(s"Failed to generate block")
    }

    context.system.scheduler.scheduleOnce(settings.blockGenerationDelay)(viewHolderRef ! GetDataFromCurrentView(actOnCurrentView))
  }

  def pickTransactions(memPool: MemPool,
                       state: State,
                       wallet: Wallet,
                       parent: Block
                      ): Try[Seq[Transaction]] = Try {

    lazy val to: PublicKey25519Proposition = PublicKey25519Proposition(wallet.secrets.head.publicImage.pubKeyBytes)

    val infVal = 0 //Await.result(infQ ? view._1.height, Duration.Inf).asInstanceOf[Long]

    // build the list of transactions to possibly be included in a block
    CoinbaseTransaction.createAndApply(wallet, IndexedSeq((to, infVal)), parent.id.hashBytes).get +:
    memPool.take(TransactionsInBlock).foldLeft(Seq[Transaction]()) { case (txAcc, tx) =>
      val txNotIncluded = tx.boxIdsToOpen.forall(id => !txAcc.flatMap(_.boxIdsToOpen).exists(_ sameElements id))
      val validBoxes = tx.newBoxes.forall(b ⇒ state.getBox(b.id).isEmpty)

      if (validBoxes) memPool.remove(tx)

      state.validate(tx) match {
        case Success(_) if txNotIncluded => txAcc :+ tx
        case Success(_)                  => txAcc
        case Failure(ex)                 =>
          log.debug(s"${Console.RED}Invalid Unconfirmed transaction $tx. Removing transaction${Console.RESET}. Failure: $ex")
          txAcc
      }
    }
  }

  def iteration(parent: Block,
                difficulty: Long,
                boxKeys: Seq[(ArbitBox, PrivateKey25519)],
                txsToInclude: Seq[Transaction],
                version: Block.Version): Option[Block] = {

    val timestamp = appContext.timeProvider.time()
    val target = calcAdjustedTarget(parent, difficulty, timestamp)

    val successfulHits = boxKeys.map { boxKey =>
      val h = calcHit(parent)(boxKey._1)
      (boxKey, h)
    }.filter(t => BigInt(t._2) < (((t._1._1.value) * target)).toBigInt)
    log.debug(s"Successful hits: ${successfulHits.size}")

    successfulHits.headOption.map { case (boxKey, _) =>
      Block.create(parent.id, timestamp, txsToInclude, boxKey._1, boxKey._2, version)
    }
  }

}

////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// COMPANION SINGLETON ////////////////////////////////

object Forger {

  object ReceivableMessages {

    case object StartForging

    case object StopForging

  }

}

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// ACTOR REF HELPER //////////////////////////////////

object ForgerRef {
  def props(nodeViewHolderRef: ActorRef, settings: ForgingSettings, appContext: AppContext)
           (implicit ec: ExecutionContext): Props =
    Props(new Forger(nodeViewHolderRef, settings, appContext))

  def apply(nodeViewHolderRef: ActorRef, settings: ForgingSettings, appContext: AppContext)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(nodeViewHolderRef, settings, appContext))

  def apply(name: String, nodeViewHolderRef: ActorRef, settings: ForgingSettings, appContext: AppContext)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(nodeViewHolderRef, settings, appContext), name)
}