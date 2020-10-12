package co.topl.nodeView.state.genesis

import akka.actor.ActorRef
import co.topl.consensus.Forger.ReceivableMessages.GenesisParams
import co.topl.crypto.PrivateKey25519
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.settings.NetworkType.{ LocalNet, MainNet }
import co.topl.settings.{ NetworkType, Version }
import co.topl.utils.Logging
import scorex.crypto.signatures.{ PrivateKey, PublicKey }

import scala.concurrent.duration.FiniteDuration
import scala.util.Try

trait GenesisProvider extends Logging {

  protected val genesisAcct: PrivateKey25519 = PrivateKey25519(PrivateKey @@ Array.fill(32)(2: Byte), PublicKey @@ Array.fill(32)(2: Byte))

  protected val blockChecksum: ModifierId

  protected val blockVersion: Version

  protected val targetBlockTime: FiniteDuration

  protected val members: Map[String, Long]

  def getGenesisBlock: Try[(Block, GenesisParams)]
}

object GenesisProvider {
  def initializeGenesis(networkType: NetworkType, keyManager: ActorRef): Try[(Block, GenesisParams)] =
    networkType match {
      case MainNet  => Toplnet.getGenesisBlock
      case LocalNet => LocalTestnet(keyManager).getGenesisBlock
      case _        => throw new Error("Undefined network type.")
    }
}
