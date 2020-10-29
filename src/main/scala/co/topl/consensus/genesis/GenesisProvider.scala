package co.topl.consensus.genesis

import co.topl.consensus.Forger.ChainParams
import co.topl.attestation.proposition.PublicKeyCurve25519Proposition
import co.topl.attestation.proof.SignatureCurve25519
import co.topl.attestation.secrets.PrivateKeyCurve25519
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.{ ArbitTransfer, PolyTransfer }
import co.topl.settings.Version
import co.topl.utils.Logging
import scorex.crypto.signatures.{ PrivateKey, PublicKey }

import scala.util.Try

trait GenesisProvider extends Logging {

  type POLY = (
    IndexedSeq[(PublicKeyCurve25519Proposition, Long)],
      IndexedSeq[(PublicKeyCurve25519Proposition, Long)],
      Map[PublicKeyCurve25519Proposition, SignatureCurve25519], Long, Long, String) => PolyTransfer

  type ARB = (
    IndexedSeq[(PublicKeyCurve25519Proposition, Long)],
      IndexedSeq[(PublicKeyCurve25519Proposition, Long)],
      Map[PublicKeyCurve25519Proposition, SignatureCurve25519], Long, Long, String) => ArbitTransfer

  protected lazy val genesisAcct: PrivateKeyCurve25519 = PrivateKeyCurve25519(PrivateKey @@ Array.fill(32)(2: Byte), PublicKey @@ Array.fill(32)(2: Byte))

  protected lazy val totalStake: Long = members.values.sum

  protected val blockChecksum: ModifierId

  protected val blockVersion: Version

  protected val initialDifficulty: Long

  protected val members: Map[String, Long]

  def getGenesisBlock: Try[(Block, ChainParams)]

}
