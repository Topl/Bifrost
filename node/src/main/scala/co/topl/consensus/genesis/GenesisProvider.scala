package co.topl.consensus.genesis

import co.topl.consensus.Forger.ChainParams
import co.topl.keyManagement.PrivateKeyCurve25519
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.block.PersistentNodeViewModifier.PNVMVersion
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.{Int128, Logging}
import co.topl.crypto.signatures.{PrivateKey, PublicKey}

import scala.util.Try

trait GenesisProvider extends Logging {

  implicit val networkPrefix: NetworkPrefix

  protected lazy val genesisAcct: PrivateKeyCurve25519 =
    PrivateKeyCurve25519(PrivateKey(Array.fill(32)(2: Byte)), PublicKey(Array.fill(32)(2: Byte)))

  protected lazy val totalStake: Int128 = members.values.sum

  protected val blockChecksum: ModifierId

  protected val blockVersion: PNVMVersion

  protected val initialDifficulty: Long

  protected val members: Map[String, Int128]

  def getGenesisBlock: Try[(Block, ChainParams)]

}
