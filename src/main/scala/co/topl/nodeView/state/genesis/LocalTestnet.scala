package co.topl.nodeView.state.genesis

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import co.topl.consensus.Forger.ReceivableMessages.{ CreateGenesisKeys, GenesisParams }
import co.topl.crypto.Signature25519
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.{ ArbitTransfer, PolyTransfer }
import co.topl.nodeView.history.History
import co.topl.nodeView.state.box.ArbitBox
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.settings.{ AppSettings, Version }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{ DurationInt, FiniteDuration }
import scala.concurrent.{ Await, Future }
import scala.util.Try

case class LocalTestnet(keyManager: ActorRef, settings: AppSettings) extends GenesisProvider {

  override protected val blockChecksum: ModifierId = ModifierId(Array.fill(32)(0: Byte))

  override protected val blockVersion: Version = settings.application.version

  protected val targetBlockTime: FiniteDuration = settings.forging.targetBlockTime

  protected val initialDifficulty: Long = settings.forging.initialDifficulty

  override protected val members: Map[String, Long] = Map("Not implemented here" -> 0L)

  override def getGenesisBlock: Try[(Block, GenesisParams)] = Try {
    Await.result(formNewBlock, timeout.duration)
  }

  /**
   * We want a private network to have a brand new genesis block that is created at runtime. This is
   * done to allow the user to forge on their private network. Therefore, we need to generate a new set of keys
   * by making a call to the key manager holder to create a the set of forging keys. Once these keys are created,
   * we can use the public images to pre-fund the accounts from genesis.
   */
  implicit val timeout: Timeout = 30 seconds
  val numberOfKeys: Int = settings.forging.numTestnetAccts.getOrElse(10)
  val balance: Long = settings.forging.testnetBalance.getOrElse(1000000L)

  def formNewBlock: Future[(Block, GenesisParams)] = {

    // send the request to get keys to the key mangers
    (keyManager ? CreateGenesisKeys(numberOfKeys)).mapTo[Set[PublicKey25519Proposition]].map { keys =>

      // map the members to their balances then continue as normal
      val privateTotalStake = numberOfKeys * balance

      val txInput = (
        IndexedSeq(genesisAcct.publicImage -> 0L),
        keys.map(_ -> balance).toIndexedSeq,
        Map(genesisAcct.publicImage -> Signature25519.genesis()),
        0L,
        0L,
        "")

      val txs = Seq((ArbitTransfer.apply: ARB).tupled(txInput), (PolyTransfer.apply: POLY).tupled(txInput))

      val generatorBox = ArbitBox(genesisAcct.publicImage, 0, privateTotalStake)

      val signature = Signature25519.genesis()

      val block = Block(History.GenesisParentId, 0L, generatorBox, signature, txs, blockVersion.blockByte)

      log.debug(s"Initialize state with transaction ${txs} with boxes ${txs.head.newBoxes}")

      (block, GenesisParams(privateTotalStake, targetBlockTime, initialDifficulty))
    }
  }
}
