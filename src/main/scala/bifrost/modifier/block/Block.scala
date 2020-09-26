package bifrost.modifier.block

import bifrost.crypto.{ FastCryptographicHash, PrivateKey25519, Signature25519 }
import bifrost.history.History
import bifrost.modifier.ModifierId
import bifrost.modifier.block.Block._
import bifrost.modifier.box.ArbitBox
import bifrost.modifier.box.serialization.BoxSerializer
import bifrost.modifier.transaction.bifrostTransaction.Transaction
import bifrost.nodeView.NodeViewModifier.ModifierTypeId
import bifrost.nodeView.{ BifrostNodeViewModifier, NodeViewModifier }
import bifrost.utils.serialization.BifrostSerializer
import io.circe.syntax._
import io.circe.{ Encoder, Json }
import scorex.util.encode.Base58
import scorex.crypto.signatures.{Curve25519, Signature}
import supertagged.@@
// fixme: JAA 0 2020.07.19 - why is protobuf still used here?
import serializer.BloomTopics

import scala.collection.BitSet

/**
 * A block is an atomic piece of data network participates are agreed on.
 *
 * A block has:
 * - transactional data: a sequence of transactions, where a transaction is an atomic state update.
 * Some metadata is possible as well(transactions Merkle tree root, state Merkle tree root etc).
 *
 * - consensus data to check whether block was generated by a right party in a right way. E.g.
 * "baseTarget" & "generatorSignature" fields in the Nxt block structure, nonce & difficulty in the
 * Bitcoin block structure.
 *
 * - a signature(s) of a block generator(s)
 *
 * - additional data: block structure version no, timestamp etc
 */

case class Block ( parentId: BlockId,
                   timestamp: Timestamp,
                   forgerBox: ArbitBox,
                   signature: Signature25519,
                   txs: Seq[Transaction],
                   version  : Version
                 ) extends BifrostNodeViewModifier {

  type M = Block

  lazy val modifierTypeId: ModifierTypeId = Block.modifierTypeId

  lazy val transactions: Option[Seq[Transaction]] = Some(txs)

  lazy val serializer: BifrostSerializer[Block] = BlockSerializer

  lazy val id: BlockId = ModifierId(serializedId)

  lazy val serializedId: Array[Byte] = {
    val blockWithoutSig = this.copy(signature = Signature25519(Signature @@ Array.emptyByteArray))
    FastCryptographicHash(serializer.toBytes(blockWithoutSig))
  }

  lazy val serializedParentId: Array[Byte] = parentId.hashBytes

  lazy val json: Json = Map(
    "id" -> Base58.encode(serializedId).asJson,
    "parentId" -> Base58.encode(serializedParentId).asJson,
    "timestamp" -> timestamp.asJson,
    "generatorBox" -> Base58.encode(BoxSerializer.toBytes(forgerBox)).asJson,
    "signature" -> Base58.encode(signature.signature).asJson,
    "txs" -> txs.map(_.json).asJson,
    "version" -> version.asJson,
    "blockSize" -> serializer.toBytes(this).length.asJson
    ).asJson
}

object Block {

  type BlockId = ModifierId
  type Timestamp = Long
  type Version = Byte
  type GenerationSignature = Array[Byte]
  type BaseTarget = Long

  val blockIdLength: Int = NodeViewModifier.ModifierIdSize
  val modifierTypeId: Byte @@ NodeViewModifier.ModifierTypeId.Tag = ModifierTypeId @@ (3: Byte)
  val signatureLength: Int = Curve25519.SignatureLength25519

  def create ( parentId  : BlockId,
               timestamp : Timestamp,
               txs       : Seq[Transaction],
               box       : ArbitBox,
               //attachment: Array[Byte],
               privateKey: PrivateKey25519,
               version   : Version
             ): Block = {

    assert(box.proposition.pubKeyBytes sameElements privateKey.publicKeyBytes)

    // generate block message (block with empty signature) to be signed
    val blockMessage = Block(parentId, timestamp, box, Signature25519(Signature @@ Array.emptyByteArray), txs, version)

    // generate signature from the block message and private key
    val signature = if ( parentId.hashBytes sameElements History.GenesisParentId ) {
      Array.fill(signatureLength)(1: Byte) // genesis block will skip signature check
    } else {
      Curve25519.sign(privateKey.privKeyBytes, blockMessage.bytes)
    }

    // return a valid block with the signature attached
    blockMessage.copy(signature = Signature25519(Signature @@ signature))
  }

  def createBloom ( txs: Seq[Transaction] ): Array[Byte] = {
    val bloomBitSet = txs.foldLeft(BitSet.empty)(
      ( total, b ) =>
        b.bloomTopics match {
          case Some(e) => total ++ Bloom.calcBloom(e.head, e.tail)
          case None    => total
        }
      ).toSeq
    BloomTopics(bloomBitSet).toByteArray
  }

  implicit val jsonEncoder: Encoder[Block] = { b: Block ⇒
    Map(
      "id" -> Base58.encode(b.serializedId).asJson,
      "parentId" -> Base58.encode(b.serializedParentId).asJson,
      "timestamp" -> b.timestamp.asJson,
      "generatorBox" -> Base58.encode(BoxSerializer.toBytes(b.forgerBox)).asJson,
      "signature" -> Base58.encode(b.signature.signature).asJson,
      "txs" -> b.txs.map(_.json).asJson,
      "version" -> b.version.asJson,
      "blockSize" -> b.serializer.toBytes(b).length.asJson
      ).asJson
  }
}
