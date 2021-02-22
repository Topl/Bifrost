package co.topl.utils

import co.topl.attestation.PrivateKeyCurve25519
import co.topl.consensus.KeyRing
import co.topl.consensus.genesis.PrivateTestnet
import co.topl.crypto.KeyfileCurve25519
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.nodeView.history.History
import co.topl.nodeView.state.State
import co.topl.settings.AppSettings

import scala.util.{Failure, Success}

trait GenesisGenerators extends CoreGenerators
  with FileUtils {

  val keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519] =
    KeyRing(settings.application.keyFileDir.get, KeyfileCurve25519)

  val genesisBlock: Block = PrivateTestnet((_: Int, _: Option[String]) => {
    keyRing.generateNewKeyPairs(num = 3) match {
      case Success(keys) => keys.map(_.publicImage)
      case Failure(ex)   => throw ex
    } }, settings).getGenesisBlock.get._1

  val genesisBlockId: ModifierId = genesisBlock.id

  def genesisState(settings: AppSettings, genesisBlockWithVersion: Block = genesisBlock): State = {
    History.readOrGenerate(settings).append(genesisBlock)
    State.genesisState(settings, Seq(genesisBlockWithVersion))
  }
}
