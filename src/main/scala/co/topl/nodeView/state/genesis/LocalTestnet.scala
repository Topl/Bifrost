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
import co.topl.settings.Version
import scorex.crypto.signatures.Signature

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.{ DurationInt, FiniteDuration }
import scala.util.Try

case class LocalTestnet(keyManager: ActorRef) extends GenesisProvider {

  override protected val blockChecksum: ModifierId = ModifierId(Array.fill(32)(0: Byte))

  override protected val blockVersion: Version = Version(0,0,1)

  override protected val targetBlockTime: FiniteDuration = 500 milliseconds

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
  val numberOfKeys = 100
  val balance = 1000000L

  def formNewBlock: Future[(Block, GenesisParams)] = {
    // send the request to get keys to the key mangers
    (keyManager ? CreateGenesisKeys(numberOfKeys)).mapTo[Set[PublicKey25519Proposition]].map { keys =>

      // map the members to their balances then continue as normal
      val privateMembers = keys.map(_ -> balance).toIndexedSeq
      val privateTotalStake = numberOfKeys * balance

      val arbTx = ArbitTransfer(
        IndexedSeq(genesisAcct.publicImage -> 0L),
        privateMembers,
        Map(genesisAcct.publicImage -> Signature25519(Signature @@ Array.fill(Signature25519.SignatureSize)(1: Byte))),
        0L,
        0L,
        "")

      val polyTx = PolyTransfer(
        IndexedSeq(genesisAcct.publicImage -> 0L),
        privateMembers,
        Map(genesisAcct.publicImage -> Signature25519(Signature @@ Array.fill(Signature25519.SignatureSize)(1: Byte))),
        0L,
        0L,
        "")

      val txs = Seq(arbTx, polyTx)

      val generatorBox = ArbitBox(genesisAcct.publicImage, 0, privateTotalStake)

      val signature = Signature25519.genesis()

      val block = Block(History.GenesisParentId, 0L, generatorBox, signature, txs, blockVersion.blockByte)

      log.debug(s"Initialize state with transaction ${txs} with boxes ${txs.head.newBoxes}")

      (block, GenesisParams(privateTotalStake, targetBlockTime))
    }
  }
}
