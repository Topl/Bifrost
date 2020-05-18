package bifrost.forging

import java.time.Instant

import akka.actor._
import bifrost.modifier.block.Block
import bifrost.history.BifrostHistory
import bifrost.mempool.MemPool
import bifrost.nodeView.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.state.State
import bifrost.modifier.box.ArbitBox
import bifrost.wallet.Wallet
import com.google.common.primitives.Longs
import bifrost.network.BifrostLocalInterface.LocallyGeneratedModifier
import bifrost.settings.Settings
import bifrost.modifier.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.utils.Logging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import akka.util.Timeout
import bifrost.modifier.block.Block.Version
import bifrost.crypto.{FastCryptographicHash, PrivateKey25519}
import bifrost.modifier.transaction.bifrostTransaction.{BifrostTransaction, CoinbaseTransaction}

import scala.util.Try

trait ForgerSettings extends Settings {
}

class Forger(forgerSettings: ForgingSettings, viewHolderRef: ActorRef) extends Actor with ActorLogging {

  import bifrost.forging.Forger._

  //should be a part of consensus, but for our app is okay
  val TransactionsInBlock = 100

  // inflation query actor
  //private val infQ = ActorSystem("infChannel").actorOf(Props[InflationQuery], "infQ")

  //set to true for initial generator
  private val initialForging = forgerSettings.offlineGeneration
  private var forging = forgerSettings.offlineGeneration

  override def preStart(): Unit = {
    if (initialForging) context.system.scheduler.scheduleOnce(1.second)(self ! StartForging)
  }

  def pickTransactions(memPool: MemPool,
                       state: State,
                       parent: Block,
                       view: (BifrostHistory, State, Wallet, MemPool)
                      ): Try[Seq[BifrostTransaction]] = Try {
    implicit val timeout: Timeout = 10 seconds
    lazy val to: PublicKey25519Proposition = PublicKey25519Proposition(view._3.secrets.head.publicImage.pubKeyBytes)
    val infVal = 0 //Await.result(infQ ? view._1.height, Duration.Inf).asInstanceOf[Long]
    lazy val CB = CoinbaseTransaction.createAndApply(view._3, IndexedSeq((to, infVal)), parent.id).get
    val regTxs = memPool.take(TransactionsInBlock).foldLeft(Seq[BifrostTransaction]()) { case (txSoFar, tx) =>
      val txNotIncluded = tx.boxIdsToOpen.forall(id => !txSoFar.flatMap(_.boxIdsToOpen).exists(_ sameElements id))
      val txValid = state.validate(tx)
      if (txValid.isFailure) {
        log.debug(s"${Console.RED}Invalid Unconfirmed transaction $tx. Removing transaction${Console.RESET}")
        txValid.failed.get.printStackTrace()
        memPool.remove(tx)
      }
      if (txValid.isSuccess && txNotIncluded) txSoFar :+ tx else txSoFar
    }
    CB +: regTxs
  }

  override def receive: Receive = {
    case StartForging =>
      if(initialForging) {
        log.info("No Better Neighbor. Forger starts forging now.")
        forging = true
        viewHolderRef ! GetCurrentView
      }

    case StopForging =>
      forging = false

    case CurrentView(h: BifrostHistory, s: State, w: Wallet, m: MemPool) =>
      self ! TryForging(h, s, w, m)

    case TryForging(h: BifrostHistory, s: State, w: Wallet, m: MemPool) =>
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

        iteration(parent, boxKeys, pickTransactions(m, s, parent, (h, s, w, m)).get, adjustedTarget, forgerSettings.version) match {
          case Some(block) =>
            log.debug(s"Locally generated block: $block")
            viewHolderRef !
              LocallyGeneratedModifier[ProofOfKnowledgeProposition[PrivateKey25519], BifrostTransaction, Block](block)
          case None =>
            log.debug(s"Failed to generate block")
        }
        context.system.scheduler.scheduleOnce(forgerSettings.blockGenerationDelay)(viewHolderRef ! GetCurrentView)
      }
  }
}

object Forger extends Logging {


  val MaxTarget = Long.MaxValue

  case object StartForging

  case object StopForging

  case class TryForging[HIS, MS, VL, MP](history: HIS, state: MS, vault: VL, pool: MP)

  def hit(lastBlock: Block)(box: ArbitBox): Long = {
    val h = FastCryptographicHash(lastBlock.bytes ++ box.bytes)
    Longs.fromByteArray((0: Byte) +: h.take(7))
  }


  def iteration(parent: Block,
                boxKeys: Seq[(ArbitBox, PrivateKey25519)],
                txsToInclude: Seq[BifrostTransaction],
                target: BigInt,
                version: Version): Option[Block] = {

    log.debug("in the iteration function")
    val successfulHits = boxKeys.map { boxKey =>
      val h = hit(parent)(boxKey._1)
      //log.debug(s"Hit value: $h")
      (boxKey, h)
    }.filter(t => BigInt(t._2) < BigInt(t._1._1.value) * target)

    log.debug(s"Successful hits: ${successfulHits.size}")
    successfulHits.headOption.map { case (boxKey, _) =>
    if (txsToInclude.head.asInstanceOf[CoinbaseTransaction].newBoxes.nonEmpty) {
        Block.create(parent.id, Instant.now().toEpochMilli, txsToInclude, boxKey._1, boxKey._2,
          txsToInclude.head.asInstanceOf[CoinbaseTransaction].newBoxes.head.asInstanceOf[ArbitBox].value, version) // inflation val
      }
    else {
        Block.create(parent.id, Instant.now().toEpochMilli, txsToInclude, boxKey._1, boxKey._2, 0, version)
      }
    }
  }

  def calcAdjustedTarget(difficulty: Long,
                         parent: Block,
                         targetBlockDelay: Long): BigInt = {
    val target: Double = MaxTarget.toDouble / difficulty.toDouble
    val timedelta = Instant.now().toEpochMilli - parent.timestamp
    BigDecimal(target * timedelta.toDouble / targetBlockDelay.toDouble).toBigInt()
  }
}
