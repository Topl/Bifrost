package co.topl.nodeView.state.genesis

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import co.topl.consensus.Forger.ReceivableMessages.CreateGenesisKeys
import co.topl.crypto.Signature25519
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.{ ArbitTransfer, PolyTransfer }
import co.topl.nodeView.history.History
import co.topl.nodeView.state.box.ArbitBox
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.settings.Version
import scorex.crypto.signatures.Signature

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.Try

case class LocalTestnet(keyManager: ActorRef) extends GenesisProvider {

  override protected val blockChecksum: ModifierId = ModifierId(Array.fill(32)(0: Byte))

  override protected val blockVersion: Version = Version(0,0,1)

  override protected val members: Map[String, Long] = Map("Not implemented here" -> 0L)

  implicit val timeout: Timeout = 30 seconds

  val numberOfKeys = 10
  val balance = 1000000L

  def getGenesisBlock: Try[Block] = Try {
    val newMembers = {
      Await.result(
        (keyManager ? CreateGenesisKeys(numberOfKeys)).mapTo[Set[PublicKey25519Proposition]],
        timeout.duration
        ).map(_ -> balance).toIndexedSeq
    }

    val arbTx = ArbitTransfer(
      IndexedSeq(genesisAcct.publicImage -> 0L),
      newMembers,
      Map(genesisAcct.publicImage -> Signature25519(Signature @@ Array.fill(Signature25519.SignatureSize)(1: Byte))),
      0L,
      0L,
      "")

    val polyTx = PolyTransfer(
      IndexedSeq(genesisAcct.publicImage -> 0L),
      newMembers,
      Map(genesisAcct.publicImage -> Signature25519(Signature @@ Array.fill(Signature25519.SignatureSize)(1: Byte))),
      0L,
      0L,
      "")

    val genesisTxs = Seq(arbTx, polyTx)

    val genesisBox = ArbitBox(genesisAcct.publicImage, 0, members.values.sum)

    val genesisBlock = Block.create(History.GenesisParentId, 0L, genesisTxs, genesisBox, genesisAcct, blockVersion.blockByte)

    log.debug(s"Initialize state with transaction ${genesisTxs.head} with boxes ${genesisTxs.head.newBoxes}")

    genesisBlock
  }
}
