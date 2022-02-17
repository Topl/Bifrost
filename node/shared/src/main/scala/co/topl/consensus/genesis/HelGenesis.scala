package co.topl.consensus.genesis

import co.topl.attestation.SignatureCurve25519
import co.topl.codecs._
import co.topl.consensus.Forger.ChainParams
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.block.PersistentNodeViewModifier.PNVMVersion
import co.topl.modifier.box.SimpleValue
import co.topl.utils.implicits._
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.{Int128, NetworkType}

import scala.collection.immutable.ListMap
import scala.util.Try

case object HelGenesis extends GenesisProvider {

  implicit val networkPrefix: NetworkPrefix = NetworkType.HelTestnet.netPrefix

  override protected val blockChecksum: ModifierId =
    Base58Data
      .unsafe("vKjyX77HLRUiihjWofSsacNEdDGMaJpNJTQMXkRyJkP2")
      .decodeTransmitted[ModifierId]
      .getOrThrow()

  override protected val blockVersion: PNVMVersion = 1: Byte

  override protected val initialDifficulty: Long = 1000000000000000000L

  override protected[genesis] val members: ListMap[String, Int128] = ListMap(
    "5jcDCMqoNEzFGdg2rmaa5fxDd1SwksSrFYQGEhB5XF1acofm2hru" -> 10000000000000000L,
    "5jbmpLZid5i6GydGYmm98eULWNJJBvstmk8qZVoKoidzrv4dMXUD" -> 10000000000000000L,
    "5jc1FLTr4m3kZAKndrfQtDRBLAMhdZAPFRDbp7RxaLH5fUhM5Jun" -> 10000000000000000L,
    "5jbChzGx54eNSPvYgtcpTMjqExeRR6SvbvEWMrsLNpTmhkQ8Ssvf" -> 10000000000000000L,
    "5jb9W76VgpZkGbaowByDHPVnPdtd3UKrDhC1XxNmuBn9z6oxMbpj" -> 10000000000000000L,
    "5jcUuZXCEeHvt8uVV9LbZkUjjhSjZrQx8A5CVvKh5SBTXy4H6HyP" -> 10000000000000000L,
    "5jcoA2BAZ27HxANAPKuCkT45TpLz4GizNfmd9HXYm5eC3zno1dKU" -> 10000000000000000L,
    "5jcKxM4HxgJUmxJRERP2BXjYivRCNo2bjPtj5iYwbLXH9odhrw9u" -> 10000000000000000L,
    "5jcco1SfWKimKbR1U6Ag7VsgraWM36i2PwnCimSdwqc6XxkumdQX" -> 10000000000000000L,
    "5jbuiYteK387qmnMQg2UDKPZQocuhFUEX7zvmvauLhoGSrQfx1xG" -> 10000000000000000L,
    "5jc35RaEcd6ivCAxrQswh2tyi4CB47VgAdpX4GBS4HPduuXUXdgh" -> 10000000000000000L,
    "5jcBCssN9KnzyW1x4qRRrFMM3WWXkbWLmjVAG9n4CUVNEbGemioJ" -> 10000000000000000L,
    "5jcuxJSuUWnhg9xQsMGFnPGWdD3tDsZKGAvCBZSn6GnSx2bHug8J" -> 10000000000000000L,
    "5jbMNP6o2gUtmfW5j7bxuiMqTeTq1XUujNnSLf72DgKQoepCP97t" -> 10000000000000000L,
    "5jbRnft1M6NHtAGE5aHJwXKw33fY6FQw6YpaXLNrS12B6B7JnzjZ" -> 10000000000000000L,
    "5jbNEm3Js83e7kNpbpsTVv7rbFnT8P6mszgcrZqYWrmQKqSQtiZh" -> 10000000000000000L,
    "5jc7fA6sutFy3XZUvzEUvbxXaAFvbN2zhahSrAcDhx9sxhuDG6ve" -> 10000000000000000L,
    "5jcXmcP7PP3iUer2fYJ28XuQZqhtMFTZCbeC2hgnxwdyVN1iFZWq" -> 10000000000000000L,
    "5jc3p41NUqJbjdmfG3EQX9iXKyrR3fUSpfahwbF4y1TuPap1ZnBB" -> 10000000000000000L,
    "5jcoHDP4L6Q2kpFrL3xTC1Psd7MkFav7TaHi2HbwCAoC6B6DcLN7" -> 10000000000000000L
  )

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
        generateGenesisTransaction(txInput),
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
}
