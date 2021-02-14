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

case object HelGenesis extends GenesisProvider {

  implicit val networkPrefix: NetworkPrefix = NetworkType.HelTestnet.netPrefix

  override protected val blockChecksum: ModifierId = ModifierId("2B6WtokYmedgQ7XXZk7b6udjXagxVTgE4QHSiqXFsmxET")

  override protected val blockVersion: PNVMVersion = 0: Byte

  override protected val initialDifficulty: Long = 1000000000000000000L

  override protected val members: Map[String, Int128] = Map(
    "5jcG6p4iF9Ay5JxdN2P8s6mpaF31ACLz2JKYjYBg1TPovLCPxhNw" -> 10000000000000000L,
    "5jcSSAL5pUkpkRJH9Gmm2bEs3nJ1PRdQfptS83MjSnuDo8W8MckU" -> 10000000000000000L,
    "5jbVMF3ocUTFKFy6Aqn83qxYzgsbVdKjRyMxLY8MAWb5s99Hto9W" -> 10000000000000000L,
    "5jbdxgmbNwjLYi16ViF47rdiyiDhcFsn4rW4UezXNk4Ke7uFUmVn" -> 10000000000000000L,
    "5jc3whAuLf4xkRfaz2o32VTaTWPSo2auUEVy2pGt71tPpzQaTgEj" -> 10000000000000000L,
    "5jcrZMwt8PGtBhEXkCzAhVkLMby8rpk4yTcnwWK9TSJWskJYtqDv" -> 10000000000000000L,
    "5jcTnzxmzzbV1PDjkjx2mmnPhDtnztLMvkq97VVBUgXbp5PzsYd9" -> 10000000000000000L,
    "5jbG91cSqm7K1W2oyTZcMc7trmmWGbBf2mm74sWQ8LFEwrNDr8we" -> 10000000000000000L,
    "5jcrZQxJHvV51gP8Zj1wbdFwqBNCmUXcroTuDsZZYHkkzqaxpM8v" -> 10000000000000000L,
    "5jcW1G2QqzFtewHfqCvGD5642WZ6wHjAfCknZQ5tiwmGqGbw49qZ" -> 10000000000000000L,
    "5jcETvKB3eP7tnpEDzjD6NR1aqsc8N9txgR4hJxNpJ9TH4LtZoD4" -> 10000000000000000L,
    "5jbHtWxsKQ6wuyAdRoQgYSjAsdtghJi7UxZM6DHpQ4T1WRApGUZj" -> 10000000000000000L,
    "5jcKbtnWgtcLUkAZuvR56WNgTazX3x22hahrNnyfYaquoUocNJJW" -> 10000000000000000L,
    "5jbocLtG3SE5e8X2nJgQzbLKgWbUg37uagYXdHmHRnCawNjAvwwk" -> 10000000000000000L,
    "5jc992DHYZ63xbjymqCgd2taLqF4taZAPSPgKKkVHEUDtENM4YVH" -> 10000000000000000L,
    "5jbtB4R1phRkfQw9bnWX8Ww46zhmaEP2tKsi4koZMmgP3ghJTthR" -> 10000000000000000L,
    "5jc6S49eNircjfjwfjn537a3gy5yXpQfzQtx3us7xTf3Myw1c5Hk" -> 10000000000000000L,
    "5jbZmmZHgh5E7iR8j4RbENbn7cqhrBrN56w6gpxx7NCtCAgq4CyF" -> 10000000000000000L,
    "5jd3edzAiXCBC3NJsDEBiN67bX3RbJRta6FhwwrEt4gRSZjuEKYU" -> 10000000000000000L,
    "5jcDW9GQvcEkVJfcX12fbqDw5fYSNxbRFSCb1cWjYXiEcM2HDmXx" -> 10000000000000000L,
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
