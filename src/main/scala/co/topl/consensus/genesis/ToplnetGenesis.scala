package co.topl.consensus.genesis

import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.attestation.EvidenceProducer.Syntax._
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.consensus.Forger.ChainParams
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.block.PersistentNodeViewModifier.PNVMVersion
import co.topl.modifier.box.{ArbitBox, SimpleValue}
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer}
import co.topl.settings.NetworkType
import co.topl.utils.Int128

import scala.util.Try

case object ToplnetGenesis extends GenesisProvider {

  implicit val networkPrefix: NetworkPrefix = NetworkType.Mainnet.netPrefix

  override protected val blockChecksum: ModifierId = ModifierId("29tx4mTsQPkynrt2KaHdtruKypx2zgWkUJr5a1knL2wGf")

  override protected val blockVersion: PNVMVersion = 0: Byte

  override protected val initialDifficulty: Long = 1000000000000000000L

  override protected val members: Map[String, Int128] = Map(
    "9cwqqgyFaBKULD937sjEBkih7bSZppWFyKZf7zTAGux13RXzjvi" -> 10000000000000000L,
    "9dmoSvdQJnofTUoCDVFuNJnUtrPamgrH2f4dVwwE1hwk19QCcVy" -> 10000000000000000L,
    "9eE3544QaZLzqTnSRBnzXvHUEc8gsMGWyZfLM4LBiyc9bYCFT6h" -> 10000000000000000L,
    "9dmGhBvmM2rv788TBL9peBgND6aCkx7rJ1hipwXuJbVFJomiJ7F" -> 10000000000000000L,
    "9dbXLo78iUk1pfU5DSzg4MnQv4mjvkYBEoGHaDXCFrvicAoU7uM" -> 10000000000000000L,
    "9e5yWT5XgwLqgGnaxYPSyArufXQRhffhKp2g84j39JwKxjojW2w" -> 10000000000000000L,
    "9ddEzyrHrBwZiUQdFThu5Er4Xrk76Hg3Tb2ZyBz4eoY3dPvpjev" -> 10000000000000000L,
    "9cqz1b9oA946KQpnWeG7HynKUTiFPFcghowGNmqete9noWEEsJH" -> 10000000000000000L,
    "9dtw2kRqeGyXGM4BQKc82SYQrfUGwR3chjSvjq6uDMqqker5wFk" -> 10000000000000000L,
    "9dyjM8mBCsQEBWgKmcoBrAFR6tLvF4rPBRkSaNK5fiUGuBe8bJb" -> 10000000000000000L,
    "9eNJDK3HRQK9Rwic2Xu9uuRKh26kJ82QwYaHSizRnCKPsRPf1Zn" -> 10000000000000000L,
    "9eHKPSaMfqjidyXnzZ3XgLJ1tZaMGtSghyyx8hX7HvzWUQcVCzp" -> 10000000000000000L,
    "9cjExrsQoYBKZ2RqnXNNfX3TyQ1rmCNdnXWWi9Ck2C3YRPj8aqB" -> 10000000000000000L,
    "9dLRHoBFgT7v6iNqNPDMEwacszxFt6UnC15r15pFgU5xHShbhtS" -> 10000000000000000L,
    "9ce8qPAyPkWaYsssA5qDFX4TUfGhGgzBkdX9dHcGsdiRK4xcqXq" -> 10000000000000000L,
    "9dRKthq14sCkMnz1mu8SiVLF1X7boH1TFxvc5RDKxewRSSXqPvQ" -> 10000000000000000L,
    "9d47mCaSjpK3TMrQBtSRhqYVrtzJapu8i6m8rDSFkkmhHg6LrLx" -> 10000000000000000L,
    "9eRz4kZupqZLRFkKzAa6NTAwqkg5LinM61o8PkvmqAhAQKbsLh4" -> 10000000000000000L,
    "9dCM1pmgpAVrQgtRemMNt12w2dmMEfnhGD55VCSixqhVoQgkg9s" -> 10000000000000000L,
    "9eTEruqCg9nESFYmaTjmdFAVckVoGoZqKmTwyeapVAtETLkiUhy" -> 10000000000000000L
  )

  def getGenesisBlock: Try[(Block, ChainParams)] = Try {

    val memberKeys = members.keys.map(Address(networkPrefix)(_))

    val txInput = (
      IndexedSeq(),
      memberKeys.zip(members.values.map(SimpleValue(_))).toIndexedSeq,
      Map(genesisAcct.publicImage -> SignatureCurve25519.genesis),
      Int128(0),
      0L,
      None,
      true
    )

    val txs = Seq(
      ArbitTransfer[PublicKeyPropositionCurve25519](
        txInput._1,
        txInput._2,
        txInput._3,
        txInput._4,
        txInput._5,
        txInput._6,
        txInput._7
      ),
      PolyTransfer[PublicKeyPropositionCurve25519](
        txInput._1,
        txInput._2,
        txInput._3,
        txInput._4,
        txInput._5,
        txInput._6,
        txInput._7
      )
    )

    val generatorBox = ArbitBox(genesisAcct.publicImage.generateEvidence, 0, SimpleValue(totalStake))

    val signature = SignatureCurve25519.genesis

    val block =
      Block(
        ModifierId.genesisParentId,
        0L,
        generatorBox,
        genesisAcct.publicImage,
        signature,
        1L,
        initialDifficulty,
        txs,
        blockVersion
      )

    require(
      block.id == blockChecksum,
      s"${Console.RED}MALFORMED GENESIS BLOCK! The calculated genesis block " +
      s"with id ${block.id} does not match the required block for the chosen network mode.${Console.RESET}"
    )

    log.debug(s"Initialize state with transaction ${txs.head} with boxes ${txs.head.newBoxes}")

    (block, ChainParams(totalStake, initialDifficulty))
  }
}
