package co.topl.consensus.genesis

import cats.implicits._
import co.topl.attestation.{Address, SignatureCurve25519}
import co.topl.consensus.Forger.ChainParams
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.block.PersistentNodeViewModifier.PNVMVersion
import co.topl.modifier.box.SimpleValue
import co.topl.settings.AppSettings
import co.topl.utils.Int128
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.implicits._

import scala.collection.immutable.ListMap
import scala.util.Try

case class PrivateGenesis(addresses: Set[Address], settings: AppSettings)(implicit
  val networkPrefix:                 NetworkPrefix
) extends GenesisProvider {

  override protected val blockChecksum: ModifierId = ModifierId.empty

  override protected val blockVersion: PNVMVersion = settings.application.version.blockByte

  override def getGenesisBlock: Try[(Block, ChainParams)] = Try(formNewBlock)

  /**
   * We want a private network to have a brand new genesis block that is created at runtime. This is
   * done to allow the user to forge on their private network. Therefore, we need to generate a new set of keys
   * by making a call to the key manager holder to create a the set of forging keys. Once these keys are created,
   * we can use the public images to pre-fund the accounts from genesis.
   */
  val (numberOfKeys, balance, initialDifficulty) = settings.forging.privateTestnet
    .map { settings =>
      (settings.numTestnetAccts, settings.testnetBalance, settings.initialDifficulty)
    }
    .getOrElse(10, 1000000L, 1000000000000000000L)

  override protected[genesis] val members: ListMap[String, Int128] = ListMap.from(
    addresses
      .map(_.show -> balance)
  )

  def formNewBlock: (Block, ChainParams) = {
    // map the members to their balances then continue as normal
    val privateTotalStake = numberOfKeys * balance

    val txInput: GenesisTransactionParams = GenesisTransactionParams(
      IndexedSeq(),
      (genesisAcctCurve25519.publicImage.address -> SimpleValue(0L)) +: addresses
        .map(_ -> SimpleValue(balance))
        .toIndexedSeq,
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
        generateGenesisTransaction(txInput),
        blockVersion
      )

    log.debug(s"Initialize state with block $block")

    (block, ChainParams(privateTotalStake, initialDifficulty))
  }
}
