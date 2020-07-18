package bifrost.consensus

import akka.actor._
import bifrost.history.History
import bifrost.mempool.MemPool
import bifrost.modifier.block.Block
import bifrost.modifier.box.ArbitBox
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.modifier.transaction.bifrostTransaction.{CoinbaseTransaction, Transaction}
import bifrost.nodeView.CurrentView
import bifrost.settings.AppSettings
import bifrost.state.State
import bifrost.utils.Logging
import bifrost.wallet.Wallet

import scala.concurrent.ExecutionContext
import scala.util.Try

class Forger(settings: AppSettings, viewHolderRef: ActorRef)
            (implicit ec: ExecutionContext) extends Actor with Logging {

  // Import the types of messages this actor can RECEIVE
  import Forger.ReceivableMessages._

  // Import the types of messages this actor can send
  import bifrost.nodeView.GenericNodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedModifier}

  val TransactionsInBlock = 100 //should be a part of consensus, but for our app is okay
  //private val infQ = ActorSystem("infChannel").actorOf(Props[InflationQuery], "infQ") // inflation query actor
  private val isForging = settings.forgingSettings.tryForging

  override def preStart(): Unit = {
    if (isForging) {
      context.system.scheduler.scheduleOnce(settings.forgingSettings.blockGenerationDelay)(self ! StartForging)
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
  def actOnCurrentView(view: CurrentView[History, State, Wallet, MemPool]): CurrentView[History, State, Wallet, MemPool] = view

  private def tryForging(h: History, s: State, w: Wallet, m: MemPool): Unit = {
      log.info(s"${Console.CYAN}Trying to generate a new block, chain length: ${h.height}${Console.RESET}")
      log.info("chain difficulty: " + h.difficulty)

      val boxes: Seq[ArbitBox] = w.boxes().filter(_.box match {
        case a: ArbitBox => s.closedBox(a.id).isDefined
        case _ => false
      }).map(_.box.asInstanceOf[ArbitBox])

      val boxKeys = boxes.flatMap(b => w.secretByPublicImage(b.proposition).map(s => (b, s)))

      val parent = h.bestBlock
      log.debug(s"Trying to generate block on top of ${parent.id} with balance " +
        s"${boxKeys.map(_._1.value).sum}")

      val adjustedTarget = calcAdjustedTarget(h.difficulty, parent, settings.forgingSettings.targetBlockTime)

      iteration(parent, boxKeys, pickTransactions(m, s, parent, (h, s, w, m)).get, adjustedTarget, settings.forgingSettings.version) match {
        case Some(block) =>
          log.debug(s"Locally generated block: $block")
          viewHolderRef !
            LocallyGeneratedModifier[Block](block)
        case None =>
          log.debug(s"Failed to generate block")
      }

      context.system.scheduler.scheduleOnce(settings.forgingSettings.blockGenerationDelay)(viewHolderRef ! GetDataFromCurrentView(actOnCurrentView))
    }

  def pickTransactions(memPool: MemPool,
                       state: State,
                       parent: Block,
                       view: (History, State, Wallet, MemPool)
                      ): Try[Seq[Transaction]] = Try {

    lazy val to: PublicKey25519Proposition = PublicKey25519Proposition(view._3.secrets.head.publicImage.pubKeyBytes)
    val infVal = 0 //Await.result(infQ ? view._1.height, Duration.Inf).asInstanceOf[Long]
    lazy val CB = CoinbaseTransaction.createAndApply(view._3, IndexedSeq((to, infVal)), parent.id.hashBytes).get
    val regTxs = memPool.take(TransactionsInBlock).foldLeft(Seq[Transaction]()) { case (txSoFar, tx) =>
      val txNotIncluded = tx.boxIdsToOpen.forall(id => !txSoFar.flatMap(_.boxIdsToOpen).exists(_ sameElements id))
      val invalidBoxes = tx.newBoxes.forall(b ⇒ state.closedBox(b.id).isEmpty)
      val txValid = state.validate(tx)
      if (txValid.isFailure) {
        log.debug(s"${Console.RED}Invalid Unconfirmed transaction $tx. Removing transaction${Console.RESET}")
        txValid.failed.get.printStackTrace()
        memPool.remove(tx)
      }
      if(!invalidBoxes) {
        memPool.remove(tx)
      }

      if (txValid.isSuccess && txNotIncluded) txSoFar :+ tx else txSoFar
    }
    CB +: regTxs
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
  def props(settings: AppSettings, nodeViewHolderRef: ActorRef)(implicit ec: ExecutionContext): Props =
    Props(new Forger(settings, nodeViewHolderRef))

  def apply(settings: AppSettings, nodeViewHolderRef: ActorRef)(implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, nodeViewHolderRef))

  def apply(name: String, settings: AppSettings, nodeViewHolderRef: ActorRef)(implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, nodeViewHolderRef), name)
}