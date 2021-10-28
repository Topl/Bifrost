package co.topl.utils.mongodb.models

import co.topl.crypto.implicits._
import co.topl.modifier.block.Block
import co.topl.utils.codecs.implicits._
import co.topl.utils.encode.Base58

case class BlockDataModel(
  id:              String,
  parentId:        String,
  timestamp:       Long,
  generatorBox:    TokenBoxDataModel,
  publicKey:       String,
  signature:       String,
  height:          Long,
  difficulty:      Long,
  txRoot:          String,
  bloomFilter:     String,
  version:         Int,
  numTransactions: Int
)

object BlockDataModel {

  def apply(block: Block): BlockDataModel =
    BlockDataModel(
      block.id.toString,
      block.parentId.toString,
      block.timestamp,
      TokenBoxDataModel(
        "ArbitBox",
        block.generatorBox.id.toString,
        block.generatorBox.nonce,
        block.generatorBox.evidence.toString,
        SimpleValueDataModel(block.generatorBox.value.quantity.toString)
      ),
      block.publicKey.toString,
      block.signature.toString,
      block.height,
      block.difficulty,
      Base58.encode(block.merkleTree.rootHash.encodeAsBase58.value),
      block.bloomFilter.toString,
      block.version,
      block.transactions.length
    )
}
