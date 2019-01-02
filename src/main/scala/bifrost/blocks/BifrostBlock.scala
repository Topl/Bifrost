package bifrost.blocks

import bifrost.transaction.box.{ArbitBox, BifrostBoxSerializer}
import bifrost.transaction.{BifrostTransaction, BifrostTransactionCompanion}
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.Json
import io.circe.syntax._
import scorex.core.NodeViewModifier.ModifierTypeId
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.ProofOfKnowledgeProposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.PrivateKey25519
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519
import serializer.BloomTopics

import scala.collection.BitSet
import scala.util.Try

case class BifrostBlock(override val parentId: BlockId,
                        override val timestamp: Block.Timestamp,
                        forgerBox: ArbitBox,
                        signature: Signature25519,
                        txs: Seq[BifrostTransaction],
                        inflation: Long)
  extends Block[ProofOfKnowledgeProposition[PrivateKey25519], BifrostTransaction] {

  override type M = BifrostBlock

  override lazy val modifierTypeId: Byte = BifrostBlock.ModifierTypeId

  override lazy val transactions: Option[Seq[BifrostTransaction]] = Some(txs)

  override lazy val serializer = BifrostBlockCompanion

  override lazy val id: BlockId = FastCryptographicHash(serializer.messageToSign(this))

  override lazy val version: Version = 0: Byte

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "parentId" -> Base58.encode(parentId).asJson,
    "timestamp" -> timestamp.asJson,
    "generatorBox" -> Base58.encode(BifrostBoxSerializer.toBytes(forgerBox)).asJson,
    "signature" -> Base58.encode(signature.signature).asJson,
    "txs" -> txs.map(_.json).asJson,
    "inflation" -> inflation.asJson
  ).asJson
}

object BifrostBlock {
  val ModifierTypeId = 3: Byte

  val SignatureLength = 64

  type GenerationSignature = Array[Byte]

  type BaseTarget = Long

  def create(parentId: BlockId,
             timestamp: Block.Timestamp,
             txs: Seq[BifrostTransaction],
             box: ArbitBox,
             //attachment: Array[Byte],
             privateKey: PrivateKey25519,
             inflation: Long): BifrostBlock = {
    assert(box.proposition.pubKeyBytes sameElements privateKey.publicKeyBytes)
    val unsigned = BifrostBlock(parentId, timestamp, box, Signature25519(Array.empty), txs, inflation)
    if (parentId sameElements Array.fill(32)(1: Byte)) {
      // genesis block will skip signature check
      val genesisSignature = Array.fill(Curve25519.SignatureLength25519)(1: Byte)
      unsigned.copy(signature = Signature25519(genesisSignature))
    } else {
      val signature = Curve25519.sign(privateKey.privKeyBytes, unsigned.bytes)
      unsigned.copy(signature = Signature25519(signature))
    }
  }

  def createBloom(txs: Seq[BifrostTransaction]): Array[Byte] = {
    val bloomBitSet = txs.foldLeft(BitSet.empty)(
      (total, b) =>
        b.bloomTopics match {
          case Some(e) => total ++ Bloom.calcBloom(e.head, e.tail)
          case None => total
        }
    ).toSeq
    BloomTopics(bloomBitSet).toByteArray
  }
}

object BifrostBlockCompanion extends Serializer[BifrostBlock] {

  def commonMessage(block: BifrostBlock): Array[Byte] = {
    val numTx = Ints.toByteArray(block.txs.length)
    val generatorBoxBytes = BifrostBoxSerializer.toBytes(block.forgerBox)

    Bytes.concat(
      block.parentId,
      Longs.toByteArray(block.timestamp),
      Longs.toByteArray(generatorBoxBytes.length),
      Array(block.version),
      generatorBoxBytes,
      Longs.toByteArray(block.inflation),
      block.signature.signature,
      numTx // writes number of transactions, then adds <tx as bytes>| <number of bytes for tx as bytes> for each tx
    )
  }

  def messageToSign(block: BifrostBlock): Array[Byte] = {
    val commonBytes = commonMessage(block)

    //noinspection ScalaStyle
    if (block.parentId sameElements Array.fill(32)(1: Byte)) {
      commonBytes ++ block.txs.foldLeft(Array[Byte]())((bytes, tx) => bytes ++ Ints.toByteArray(BifrostTransactionCompanion.toBytes(tx).length) ++ tx.messageToSign)
    } else {
      commonBytes ++ block.txs.foldLeft(Array[Byte]())((bytes, tx) => bytes ++ Ints.toByteArray(BifrostTransactionCompanion.toBytes(tx).length) ++ BifrostTransactionCompanion.toBytes(tx))
    }
  }

  override def toBytes(block: BifrostBlock): Array[Byte] = {
    commonMessage(block) ++ block.txs.foldLeft(Array[Byte]())((bytes, tx) =>
      bytes ++
        Ints.toByteArray(BifrostTransactionCompanion.toBytes(tx).length) ++
        BifrostTransactionCompanion.toBytes(tx))
  }

  override def parseBytes(bytes: Array[ModifierTypeId]): Try[BifrostBlock] = Try {

    val parentId = bytes.slice(0, Block.BlockIdLength)

    val Array(timestamp: Long, generatorBoxLen: Long) = (0 until 2).map {
      i => Longs.fromByteArray(bytes.slice(Block.BlockIdLength + i*Longs.BYTES, Block.BlockIdLength + (i + 1)*Longs.BYTES))
    }.toArray

    val version = bytes.slice(Block.BlockIdLength + Longs.BYTES, Block.BlockIdLength + Longs.BYTES + 1).head

    var numBytesRead = Block.BlockIdLength + Longs.BYTES*2 + 1

    val generatorBox = BifrostBoxSerializer.parseBytes(bytes.slice(numBytesRead, numBytesRead + generatorBoxLen.toInt)).get.asInstanceOf[ArbitBox]

    val inflation = bytes.slice(numBytesRead + generatorBoxLen.toInt, numBytesRead + generatorBoxLen.toInt + Longs.BYTES)

    val signature = Signature25519(bytes.slice(numBytesRead + generatorBoxLen.toInt + Longs.BYTES,
      numBytesRead + generatorBoxLen.toInt + Longs.BYTES + Signature25519.SignatureSize))

    numBytesRead += generatorBoxLen.toInt + Signature25519.SignatureSize + Longs.BYTES

    val numTxExpected = Ints.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Ints.BYTES))
    numBytesRead += Ints.BYTES

    def unfoldLeft[A,B](seed: B)(f: B => Option[(A, B)]): Seq[A] = {
      f(seed) match {
        case Some((a, b)) => a +: unfoldLeft(b)(f)
        case None => Nil
      }
    }

    val txBytes: Array[Byte] = bytes.slice(numBytesRead, bytes.length)

    val txByteSeq: Seq[Array[Byte]] = unfoldLeft(txBytes) {
      case b if b.length < Ints.BYTES => None
      case b =>
        val bytesToGrab = Ints.fromByteArray(b.take(Ints.BYTES))

        if (b.length - Ints.BYTES < bytesToGrab) {
          None // we're done because we can't grab the number of bytes required
        } else {
          val thisTx: Array[Byte] = b.slice(Ints.BYTES, Ints.BYTES + bytesToGrab)
          Some((thisTx, b.slice(Ints.BYTES + bytesToGrab, b.length)))
        }
    }.ensuring(_.length == numTxExpected)

    val tx: Seq[BifrostTransaction] = txByteSeq.map(tx => BifrostTransactionCompanion.parseBytes(tx).get)

    BifrostBlock(parentId, timestamp, generatorBox, signature, tx, Longs.fromByteArray(inflation))
  }
}