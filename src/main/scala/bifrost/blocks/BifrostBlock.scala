package bifrost.blocks

import bifrost.transaction.box.{ArbitBox, BifrostBoxSerializer}
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.Json
import io.circe.syntax._
import bifrost.NodeViewModifier.ModifierTypeId
import bifrost.block.Block
import bifrost.block.Block._
import bifrost.crypto.hash.FastCryptographicHash
import bifrost.serialization.Serializer
import bifrost.transaction.bifrostTransaction.BifrostTransaction
import bifrost.transaction.box.proposition.ProofOfKnowledgeProposition
import bifrost.transaction.proof.Signature25519
import bifrost.transaction.serialization.BifrostTransactionCompanion
import bifrost.transaction.state.PrivateKey25519
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519
import serializer.BloomTopics

import scala.annotation.tailrec
import scala.collection.BitSet
import scala.util.Try


case class BifrostBlock(override val parentId: BlockId,
                        override val timestamp: Block.Timestamp,
                        forgerBox: ArbitBox,
                        signature: Signature25519,
                        txs: Seq[BifrostTransaction],
                        inflation: Long = 0L,
                        protocolVersion: Version)
  extends Block[ProofOfKnowledgeProposition[PrivateKey25519], BifrostTransaction] {

  override type M = BifrostBlock

  override lazy val modifierTypeId: Byte = BifrostBlock.ModifierTypeId

  override lazy val transactions: Option[Seq[BifrostTransaction]] = Some(txs)

  override lazy val serializer = BifrostBlockCompanion

  override lazy val version: Version = protocolVersion

  override lazy val id: BlockId = FastCryptographicHash(serializer.messageToSign(this))

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "parentId" -> Base58.encode(parentId).asJson,
    "timestamp" -> timestamp.asJson,
    "generatorBox" -> Base58.encode(BifrostBoxSerializer.toBytes(forgerBox)).asJson,
    "signature" -> Base58.encode(signature.signature).asJson,
    "txs" -> txs.map(_.json).asJson,
    "inflation" -> inflation.asJson,
    "version" -> version.asJson,
    "blockSize" -> serializer.toBytes(this).length.asJson
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
             inflation: Long,
             version: Version): BifrostBlock = {
    assert(box.proposition.pubKeyBytes sameElements privateKey.publicKeyBytes)

    val unsigned = BifrostBlock(parentId, timestamp, box, Signature25519(Array.empty), txs, inflation, version)
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

  def commonMessage2xAndBefore(block: BifrostBlock): Array[Byte] = {
    val numTx = Ints.toByteArray(block.txs.length)
    val generatorBoxBytes = BifrostBoxSerializer.toBytes(block.forgerBox)

    Bytes.concat(
      block.parentId,
      Longs.toByteArray(block.timestamp),
      Longs.toByteArray(generatorBoxBytes.length),
      Array(block.version),
      generatorBoxBytes,
      block.signature.signature,
      numTx // writes number of transactions, then adds <tx as bytes>| <number of bytes for tx as bytes> for each tx
    )
  }

  def messageToSign(block: BifrostBlock): Array[Byte] = {
    val commonBytes: Array[Byte] = {
      block.version match {
        case 0 => commonMessage2xAndBefore(block)
        case _ => commonMessage(block)
      }
    }
    //noinspection ScalaStyle
    if (block.parentId sameElements Array.fill(32)(1: Byte)) {
      commonBytes ++ block.txs.foldLeft(Array[Byte]())((bytes, tx) => bytes ++ Ints.toByteArray(BifrostTransactionCompanion.toBytes(tx).length) ++ tx.messageToSign)
    } else {
      commonBytes ++ block.txs.foldLeft(Array[Byte]())((bytes, tx) => bytes ++ Ints.toByteArray(BifrostTransactionCompanion.toBytes(tx).length) ++ BifrostTransactionCompanion.toBytes(tx))
    }
  }

  override def toBytes(block: BifrostBlock): Array[Byte] = {
    block.version match {
      case 0 =>
        commonMessage2xAndBefore(block) ++ block.txs.foldLeft(Array[Byte]())((bytes, tx) =>
          bytes ++
            Ints.toByteArray(BifrostTransactionCompanion.toBytes(tx).length) ++
            BifrostTransactionCompanion.toBytes(tx))
      case _ =>
        commonMessage(block) ++ block.txs.foldLeft(Array[Byte]())((bytes, tx) =>
          bytes ++
            Ints.toByteArray(BifrostTransactionCompanion.toBytes(tx).length) ++
            BifrostTransactionCompanion.toBytes(tx))
    }
  }

  override def parseBytes(bytes: Array[ModifierTypeId]): Try[BifrostBlock] = Try {

    val parentId = bytes.slice(0, Block.BlockIdLength)

    val Array(timestamp: Long, generatorBoxLen: Long) = (0 until 2).map {
      i => Longs.fromByteArray(bytes.slice(Block.BlockIdLength + i*Longs.BYTES, Block.BlockIdLength + (i + 1)*Longs.BYTES))
    }.toArray

    val version = bytes.slice(Block.BlockIdLength + 2*Longs.BYTES, Block.BlockIdLength + 2*Longs.BYTES + 1).head

    var numBytesRead = Block.BlockIdLength + Longs.BYTES*2 + 1

    val generatorBox = BifrostBoxSerializer.parseBytes(bytes.slice(numBytesRead, numBytesRead + generatorBoxLen.toInt)).get.asInstanceOf[ArbitBox]

    val inflation = bytes.slice(numBytesRead + generatorBoxLen.toInt, numBytesRead + generatorBoxLen.toInt + Longs.BYTES)

    val signature = Signature25519(bytes.slice(numBytesRead + generatorBoxLen.toInt + Longs.BYTES,
      numBytesRead + generatorBoxLen.toInt + Longs.BYTES + Signature25519.SignatureSize))

    numBytesRead += generatorBoxLen.toInt + Signature25519.SignatureSize + Longs.BYTES

    val numTxExpected = Ints.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Ints.BYTES))
    numBytesRead += Ints.BYTES

    def unfoldLeft[A,B](seed: B)(f: B => Option[(B, A)]): Seq[A] = {
      @tailrec
      def loop(seed: B)(ls: Seq[A]): Seq[A] = f(seed) match {
        case Some((b, a)) => loop(b)(a +: ls)
        case None => ls
      }
      loop(seed)(Nil)
    }.reverse

    val txBytes: Array[Byte] = bytes.slice(numBytesRead, bytes.length)

    val txByteSeq: Seq[Array[Byte]] = unfoldLeft(txBytes) {
      case b if b.length < Ints.BYTES => None
      case b =>
        val bytesToGrab = Ints.fromByteArray(b.take(Ints.BYTES))

        if (b.length - Ints.BYTES < bytesToGrab) {
          None // we're done because we can't grab the number of bytes required
        } else {
          val thisTx: Array[Byte] = b.slice(Ints.BYTES, Ints.BYTES + bytesToGrab)
          Some((b.slice(Ints.BYTES + bytesToGrab, b.length), thisTx))
        }
    }.ensuring(_.length == numTxExpected)

    val tx: Seq[BifrostTransaction] = txByteSeq.map(tx => BifrostTransactionCompanion.parseBytes(tx).get)

    BifrostBlock(parentId, timestamp, generatorBox, signature, tx, Longs.fromByteArray(inflation), version)
  }


  def parseBytes2xAndBefore(bytes: Array[ModifierTypeId]): Try[BifrostBlock] = Try {
    val parentId = bytes.slice(0, Block.BlockIdLength)
    val Array(timestamp: Long, generatorBoxLen: Long) = (0 until 2).map {
      i => Longs.fromByteArray(bytes.slice(Block.BlockIdLength + i * Longs.BYTES, Block.BlockIdLength + (i + 1) * Longs.BYTES))
    }.toArray

    val version = bytes.slice(Block.BlockIdLength + 2*Longs.BYTES, Block.BlockIdLength + 2*Longs.BYTES + 1).head

    var numBytesRead = Block.BlockIdLength + Longs.BYTES * 2 + 1

    val generatorBox = BifrostBoxSerializer.parseBytes(bytes.slice(numBytesRead, numBytesRead + generatorBoxLen.toInt)).get.asInstanceOf[ArbitBox]
    val signature = Signature25519(bytes.slice(numBytesRead + generatorBoxLen.toInt, numBytesRead + generatorBoxLen.toInt + Signature25519.SignatureSize))

    numBytesRead += generatorBoxLen.toInt + Signature25519.SignatureSize

    val numTxExpected = Ints.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Ints.BYTES))
    numBytesRead += Ints.BYTES

    def unfoldLeft[A,B](seed: B)(f: B => Option[(B, A)]): Seq[A] = {
      @tailrec
      def loop(seed: B)(ls: Seq[A]): Seq[A] = f(seed) match {
        case Some((b, a)) => loop(b)(a +: ls)
        case None => ls
      }
      loop(seed)(Nil)
    }.reverse

    val txBytes: Array[Byte] = bytes.slice(numBytesRead, bytes.length)

    val txByteSeq: Seq[Array[Byte]] = unfoldLeft(txBytes) {
      case b if b.length < Ints.BYTES => None
      case b =>
        val bytesToGrab = Ints.fromByteArray(b.take(Ints.BYTES))

        if (b.length - Ints.BYTES < bytesToGrab) {
          None // we're done because we can't grab the number of bytes required
        } else {
          val thisTx: Array[Byte] = b.slice(Ints.BYTES, Ints.BYTES + bytesToGrab)
          Some((b.slice(Ints.BYTES + bytesToGrab, b.length), thisTx))
        }
    }.ensuring(_.length == numTxExpected)

    val tx: Seq[BifrostTransaction] = txByteSeq.map(tx => BifrostTransactionCompanion.parseBytes(tx).get)

    BifrostBlock(parentId, timestamp, generatorBox, signature, tx, protocolVersion = version)
  }
}