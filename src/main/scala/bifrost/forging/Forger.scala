package bifrost.forging

import java.time.Instant

import akka.actor.{Actor, ActorLogging, ActorRef}
import bifrost.blocks.BifrostBlock
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.state.BifrostState
import bifrost.transaction.BifrostTransaction
import bifrost.transaction.box.ArbitBox
import bifrost.wallet.BWallet
import com.google.common.primitives.Longs
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.ProofOfKnowledgeProposition
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

trait ForgerSettings extends Settings {
}

class Forger(forgerSettings: ForgingSettings, viewHolderRef: ActorRef) extends Actor with ActorLogging {

  import bifrost.forging.Forger._

  //should be a part of consensus, but for our app is okay
  val TransactionsInBlock = 100

  //set to true for initial generator
  private var forging = forgerSettings.offlineGeneration

  private val hash = FastCryptographicHash

  override def preStart(): Unit = {
    if (forging) context.system.scheduler.scheduleOnce(1.second)(self ! StartForging)
  }

  def pickTransactions(memPool: BifrostMemPool, state: BifrostState): Seq[BifrostTransaction] =
    memPool.take(TransactionsInBlock).foldLeft(Seq[BifrostTransaction]()) { case (txSoFar, tx) =>
      val txNotIncluded = tx.boxIdsToOpen.forall(id => !txSoFar.flatMap(_.boxIdsToOpen).exists(_ sameElements id))
      val txValid = state.validate(tx)
      if (txValid.isFailure) {
        log.debug(s"${Console.RED}Invalid Unconfirmed transaction $tx. Removing transaction${Console.RESET}")
        txValid.failed.get.printStackTrace()
        memPool.remove(tx)
      }
      if (txValid.isSuccess && txNotIncluded) txSoFar :+ tx
      else txSoFar
    }

  private def bounded(value: BigInt, min: BigInt, max: BigInt): BigInt =
    if (value < min) min else if (value > max) max else value

  override def receive: Receive = {
    case StartForging =>
      if (forging) {
        log.info("No Better Neighbor. Forger starts forging now.")
        forging = true
        viewHolderRef ! GetCurrentView
      } else {
        print("Tried forging but forging is disabled")
      }

    case StopForging =>
      forging = false

    case CurrentView(h: BifrostHistory, s: BifrostState, w: BWallet, m: BifrostMemPool) =>
      self ! TryForging(h, s, w, m)

    case TryForging(h: BifrostHistory, s: BifrostState, w: BWallet, m: BifrostMemPool) =>
      if (forging) {
        log.info(s"${Console.CYAN}Trying to generate a new block, chain length: ${h.height}${Console.RESET}")
        log.info("chain difficulty: " + h.difficulty)

        val boxes: Seq[ArbitBox] = w.boxes().filter(_.box match {
          case a: ArbitBox => s.closedBox(a.id).isDefined
          case _ => false
        }).map(_.box.asInstanceOf[ArbitBox])

        val boxKeys = boxes.flatMap(b => w.secretByPublicImage(b.proposition).map(s => (b, s)))

        val parent = h.bestBlock
        log.debug(s"Trying to generate block on top of ${parent.encodedId} with balance " +
          s"${boxKeys.map(_._1.value).sum}")

        val adjustedTarget = calcAdjustedTarget(h.difficulty, parent, forgerSettings.targetBlockTime.length)

        iteration(parent, boxKeys, pickTransactions(m, s), adjustedTarget) match {
          case Some(block) =>
            log.debug(s"Locally generated block: $block")
            viewHolderRef !
              LocallyGeneratedModifier[ProofOfKnowledgeProposition[PrivateKey25519], BifrostTransaction, BifrostBlock](block)
          case None =>
            log.debug(s"Failed to generate block")
        }
        context.system.scheduler.scheduleOnce(forgerSettings.blockGenerationDelay)(viewHolderRef ! GetCurrentView)
      }
  }
}

object Forger extends ScorexLogging {


  val MaxTarget = Long.MaxValue

  case object StartForging

  case object StopForging

  case class TryForging[HIS, MS, VL, MP](history: HIS, state: MS, vault: VL, pool: MP)

  def hit(lastBlock: BifrostBlock)(box: ArbitBox): Long = {
    val h = FastCryptographicHash(lastBlock.bytes ++ box.bytes)
    Longs.fromByteArray((0: Byte) +: h.take(7))
  }

  def iteration(parent: BifrostBlock,
                boxKeys: Seq[(ArbitBox, PrivateKey25519)],
                txsToInclude: Seq[BifrostTransaction],
                target: BigInt): Option[BifrostBlock] = {

    log.debug("in the iteration function")
    val successfulHits = boxKeys.map { boxKey =>
      val h = hit(parent)(boxKey._1)
      log.debug(s"Hit value: $h")
      (boxKey, h)
    }.filter(t => BigInt(t._2) < BigInt(t._1._1.value) * target)

    log.debug(s"Successful hits: ${successfulHits.size}")
    successfulHits.headOption.map { case (boxKey, _) =>
      BifrostBlock.create(parent.id, Instant.now().toEpochMilli, txsToInclude, boxKey._1, boxKey._2)
    }
  }

  def calcAdjustedTarget(difficulty: Long,
                         parent: BifrostBlock,
                         targetBlockDelay: Long): BigInt = {
    val target: Double = MaxTarget.toDouble / difficulty.toDouble
    val timedelta = Instant.now().toEpochMilli - parent.timestamp
    BigDecimal(target * timedelta.toDouble / targetBlockDelay.toDouble).toBigInt()
  }
}