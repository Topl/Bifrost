package co.topl.consensus.genesis

import co.topl.attestation.{PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.consensus.Forger.ChainParams
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.block.PersistentNodeViewModifier.PNVMVersion
import co.topl.modifier.box.SimpleValue
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer, TransferTransaction}
import co.topl.settings.GenesisFromConfigSettings
import co.topl.utils.NetworkType.{NetworkPrefix, ValhallaTestnet}
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.{Int128, NetworkType}

import scala.collection.immutable.ListMap
import scala.util.Try

case class GenesisFromConfig(settings: GenesisFromConfigSettings, networkType: NetworkType) extends GenesisProvider {

  implicit val networkPrefix: NetworkPrefix = networkType.netPrefix

  override protected val blockChecksum: ModifierId =
    ModifierId.fromBase58(Base58Data.unsafe(settings.blockChecksum))

  override protected val blockVersion: PNVMVersion = settings.blockVersion.toByte

  override protected val initialDifficulty: Long = settings.initialDifficulty

  override protected[genesis] val members: ListMap[String, Int128] =
    ListMap(settings.memberAddresses.zip(settings.memberStakes.map(Int128(_))): _*)

  def getGenesisBlock: Try[(Block, ChainParams)] = Try {

    val txInput: GenesisTransactionParams = GenesisTransactionParams(
      IndexedSeq(),
      memberKeys.zip(members.values.map(SimpleValue(_))).toIndexedSeq,
      ListMap(genesisAcctCurve25519.publicImage -> SignatureCurve25519.genesis),
      Int128(0),
      0L,
      None,
      minting = true
    )

    val block =
      Block(
        ModifierId.genesisParentId,
        0L,
        generatorBox,
        genesisAcctCurve25519.publicImage,
        signature,
        1L,
        initialDifficulty,
        if (networkPrefix == ValhallaTestnet.netPrefix) generateValhallaGenesisTransaction(txInput)
        else generateGenesisTransaction(txInput),
        blockVersion
      )

    require(
      block.id == blockChecksum,
      s"${Console.RED}MALFORMED GENESIS BLOCK! The calculated genesis block " +
      s"with id ${block.id} does not match the required block for the chosen network mode.${Console.RESET}"
    )

    log.debug(s"Initialize state with block $block")

    (block, ChainParams(totalStake, initialDifficulty))
  }

  /**
   * JAA - 2021.08.12 - There was a bug present in the Valhalla genesis block due to an unspecified feeChangeOutput in
   * the 'to' field of the Arbit transfer. To maintain compatibility with the chain we overrise the correct method to
   * the previous incorrect implementation.
   */
  protected def generateValhallaGenesisTransaction(
    params: GenesisTransactionParams
  ): Seq[TransferTransaction[SimpleValue, PublicKeyPropositionCurve25519]] =
    Seq(
      ArbitTransfer[PublicKeyPropositionCurve25519](
        params.from,
        params.to,
        params.signatures,
        params.fee,
        params.timestamp,
        params.data,
        params.minting
      ),
      PolyTransfer[PublicKeyPropositionCurve25519](
        params.from,
        params.to,
        params.signatures,
        params.fee,
        params.timestamp,
        params.data,
        params.minting
      )
    )
}
