package co.topl.nodeView.state.genesis

import akka.actor.ActorRef
import co.topl.consensus.Forger.ReceivableMessages.GenesisParams
import co.topl.crypto.{PrivateKey25519, Signature25519}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer}
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.settings.NetworkType.{LocalNet, MainNet}
import co.topl.settings.{AppSettings, NetworkType, Version}
import co.topl.utils.Logging
import scorex.crypto.signatures.{PrivateKey, PublicKey}

import scala.concurrent.duration.FiniteDuration
import scala.util.Try

trait GenesisProvider extends Logging {

  type POLY = (
    IndexedSeq[(PublicKey25519Proposition, Long)],
      IndexedSeq[(PublicKey25519Proposition, Long)],
      Map[PublicKey25519Proposition, Signature25519], Long, Long, String) => PolyTransfer

  type ARB = (
    IndexedSeq[(PublicKey25519Proposition, Long)],
      IndexedSeq[(PublicKey25519Proposition, Long)],
      Map[PublicKey25519Proposition, Signature25519], Long, Long, String) => ArbitTransfer

  protected lazy val genesisAcct: PrivateKey25519 = PrivateKey25519(PrivateKey @@ Array.fill(32)(2: Byte), PublicKey @@ Array.fill(32)(2: Byte))

  protected lazy val totalStake: Long = members.values.sum

  protected val blockChecksum: ModifierId

  protected val blockVersion: Version

  protected val targetBlockTime: FiniteDuration

  protected val initialDifficulty: Long

  protected val members: Map[String, Long]

  def getGenesisBlock: Try[(Block, GenesisParams)]

}

object GenesisProvider {
  /** Return the correct genesis parameters for the chosen network. NOTE: the default private network is set
   * in AppContext so the fall-through should result in an error.*/
  def initializeGenesis(keyManager: ActorRef, settings: AppSettings, networkType: NetworkType): Try[(Block, GenesisParams)] =
    networkType match {
      case MainNet  => Toplnet.getGenesisBlock
      case LocalNet => LocalTestnet(keyManager, settings).getGenesisBlock
      case _        => throw new Error("Undefined network type.")
    }
}
