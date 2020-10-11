package co.topl.nodeView.state.genesis

import co.topl.crypto.PrivateKey25519
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.settings.NetworkType.MainNet
import co.topl.settings.{NetworkType, Version}
import co.topl.utils.Logging

import scala.util.Try

trait GenesisProvider extends Logging {

  protected val genesisAcct: PrivateKey25519

  protected val blockChecksum: ModifierId

  protected val blockVersion: Version

  protected val members: Map[String, Long]

  def getGenesisBlock: Try[Block]
}

object GenesisProvider {
  def initializeGenesis(networkType: NetworkType): Try[Block] = networkType match {
    case MainNet => Toplnet.getGenesisBlock
    case _       => throw new Error("Undefined network type.")
  }
}
