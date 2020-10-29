package co.topl.consensus.genesis

import co.topl.consensus.Forger.ChainParams
import co.topl.attestation.proposition.PublicKeyCurve25519Proposition
import co.topl.attestation.proof.SignatureCurve25519
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.{ ArbitTransfer, PolyTransfer }
import co.topl.nodeView.history.History
import co.topl.nodeView.state.box.ArbitBox
import co.topl.settings.{ AppSettings, Version }
import co.topl.settings.{ AppSettings, RuntimeOpts, Version }

import scala.util.Try

case class PrivateTestnet ( keyGen  : (Int, Option[String]) => Set[PublicKeyCurve25519Proposition],
                            settings: AppSettings,
                            opts    : RuntimeOpts
                          ) extends GenesisProvider {

  override protected val blockChecksum: ModifierId = ModifierId(Array.fill(32)(0: Byte))

  override protected val blockVersion: Version = settings.application.version

  override protected val members: Map[String, Long] = Map("Not implemented here" -> 0L)

  override def getGenesisBlock: Try[(Block, ChainParams)] = Try(formNewBlock)

  /**
   * We want a private network to have a brand new genesis block that is created at runtime. This is
   * done to allow the user to forge on their private network. Therefore, we need to generate a new set of keys
   * by making a call to the key manager holder to create a the set of forging keys. Once these keys are created,
   * we can use the public images to pre-fund the accounts from genesis.
   */
  val (numberOfKeys, balance, initialDifficulty) = settings.forging.privateTestnet.map { settings =>
      (settings.numTestnetAccts, settings.testnetBalance, settings.initialDifficulty)
  }.getOrElse(10, 1000000L, 1000000000000000000L)

  def formNewBlock: (Block, ChainParams) = {
    // map the members to their balances then continue as normal
    val privateTotalStake = numberOfKeys * balance

    val txInput = (
      IndexedSeq(genesisAcct.publicImage -> 0L),
      keyGen(numberOfKeys, opts.seed).map(_ -> balance).toIndexedSeq,
      Map(genesisAcct.publicImage -> SignatureCurve25519.genesis()),
      0L,
      0L,
      "")

    val txs = Seq((ArbitTransfer.apply: ARB).tupled(txInput), (PolyTransfer.apply: POLY).tupled(txInput))

    val generatorBox = ArbitBox(genesisAcct.publicImage, 0, privateTotalStake)

    val signature = SignatureCurve25519.genesis()

    val block = Block(History.GenesisParentId, 0L, generatorBox, signature, txs, blockVersion.blockByte)

    log.debug(s"Initialize state with transactions ${txs}")

    (block, ChainParams(privateTotalStake, initialDifficulty))
  }
}
