package examples.bifrost.forging

import java.time.Instant

import akka.actor.{Actor, ActorRef}
import com.google.common.primitives.Longs
import examples.bifrost.blocks.BifrostBlock
import examples.bifrost.history.BifrostHistory
import examples.bifrost.mempool.BifrostMemPool
import examples.bifrost.state.BifrostState
import examples.bifrost.transaction.BifrostTransaction
import examples.bifrost.transaction.box.PolyBox
import examples.bifrost.wallet.BWallet
import scorex.core.transaction.box.proposition.ProofOfKnowledgeProposition
import scorex.core.LocalInterface.LocallyGeneratedModifier
import examples.bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.utils.{NetworkTime, ScorexLogging}
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

trait ForgerSettings extends Settings {
}

class Forger(forgerSettings: ForgingSettings, viewHolderRef: ActorRef) extends Actor with ScorexLogging {

  import Forger._

  //should be a part of consensus, but for our app is okay
  val TransactionsInBlock = 100

  //set to true for initial generator
  private var forging = forgerSettings.offlineGeneration

  private val hash = FastCryptographicHash


  val InterBlocksDelay = 15 //in seconds

  override def preStart(): Unit = {
    if (forging) context.system.scheduler.scheduleOnce(1.second)(self ! StartForging)
  }

  def pickTransactions(memPool: BifrostMemPool, state: BifrostState): Seq[BifrostTransaction] =
    memPool.take(TransactionsInBlock).foldLeft(Seq[BifrostTransaction]()) { case (txSoFar, tx) =>
      val txNotIncluded = tx.boxIdsToOpen.forall(id => !txSoFar.flatMap(_.boxIdsToOpen).exists(_ sameElements id))
      if (state.validate(tx).isSuccess && txNotIncluded) txSoFar :+ tx
      else txSoFar
    }

  private def bounded(value: BigInt, min: BigInt, max: BigInt): BigInt =
    if (value < min) min else if (value > max) max else value

  override def receive: Receive = {
    case StartForging =>
      forging = true
      viewHolderRef ! GetCurrentView

    case StopForging =>
      forging = false

    case CurrentView(h: BifrostHistory, s: BifrostState, w: BWallet, m: BifrostMemPool) =>
      log.info("Trying to generate a new block, chain length: " + h.height)

      val boxes = w.boxes().filter(_.box.isInstanceOf[PolyBox]).map(_.box.asInstanceOf[PolyBox]).filter(box => s.closedBox(box.id).isDefined)
      val boxKeys = boxes.flatMap(b => w.secretByPublicImage(b.proposition).map(s => (b, s)))

      val parent = h.bestBlock
      log.debug(s"Trying to generate block on top of ${parent.encodedId} with balance " +
        s"${boxKeys.map(_._1.value).sum}")

      val adjustedTarget = calcAdjustedTarget(h.difficulty, parent, forgerSettings.blockGenerationDelay.length)
      println(s"${Console.BLUE}Adjusted Target: $adjustedTarget")
      println(s"${Console.RESET} ")

      s.changes(parent).get.boxIdsToRemove.foreach(tr => println(s"${Console.RED}Trying to remove boxes ${Base58.encode(tr)}${Console.RESET}"))

      iteration(parent, boxKeys, pickTransactions(m, s), adjustedTarget) match {
        case Some(block) =>
          log.debug(s"Locally generated block: $block")
          forging = false
          viewHolderRef !
            LocallyGeneratedModifier[ProofOfKnowledgeProposition[PrivateKey25519], BifrostTransaction, BifrostBlock](block)
        case None =>
          log.debug(s"Failed to generate block")
      }
      context.system.scheduler.scheduleOnce(forgerSettings.blockGenerationDelay/3)(self ! StartForging)
  }
}

object Forger extends ScorexLogging {


  val MaxTarget = Long.MaxValue

  case object StartForging

  case object StopForging

  def hit(lastBlock: BifrostBlock)(box: PolyBox): Long = {
    val h = FastCryptographicHash(lastBlock.bytes ++ box.bytes)
    Longs.fromByteArray((0: Byte) +: h.take(7))
  }

  def iteration(parent: BifrostBlock,
                boxKeys: Seq[(PolyBox, PrivateKey25519)],
                txsToInclude: Seq[BifrostTransaction],
                target: BigInt): Option[BifrostBlock] = {

    log.debug("in the iteration function")
    val successfulHits = boxKeys.map { boxKey =>
      val h = hit(parent)(boxKey._1)
      log.info(s"Hit value: $h")
      (boxKey, h)
    }.filter(t => BigInt(t._2) < BigInt(t._1._1.value) * target)

    log.info(s"Successful hits: ${successfulHits.size}")
    successfulHits.headOption.map { case (boxKey, _) =>
      BifrostBlock.create(parent.id, Instant.now().toEpochMilli, txsToInclude, boxKey._1, boxKey._2)
    }
  }

  def calcAdjustedTarget(difficulty: Long,
                         parent: BifrostBlock,
                         targetBlockDelay: Long): BigInt = {
    val target: Double = MaxTarget.toDouble / difficulty.toDouble
    val timedelta = Instant.now().toEpochMilli - parent.timestamp
    println(BigDecimal(target * timedelta.toDouble / targetBlockDelay.toDouble))
    BigDecimal(target * timedelta.toDouble / targetBlockDelay.toDouble).toBigInt()
  }
}