package bifrost.modifier.block

import bifrost.serialization.Serializer
import bifrost.modifier.transaction.bifrostTransaction.BifrostTransaction
import bifrost.modifier.transaction.serialization.BifrostTransactionCompanion
import bifrost.nodeView.NodeViewModifier.ModifierTypeId
import bifrost.crypto.Signature25519
import bifrost.modifier.box.{ArbitBox, BoxSerializer}
import com.google.common.primitives.{Bytes, Ints, Longs}

import scala.annotation.tailrec
import scala.util.Try

object BlockCompanion extends Serializer[Block] {

  def commonMessage(block: Block): Array[Byte] = {
    val numTx = Ints.toByteArray(block.txs.length)
    val generatorBoxBytes = BoxSerializer.toBytes(block.forgerBox)

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

  def commonMessage2xAndBefore(block: Block): Array[Byte] = {
    val numTx = Ints.toByteArray(block.txs.length)
    val generatorBoxBytes = BoxSerializer.toBytes(block.forgerBox)

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

  def messageToSign(block: Block): Array[Byte] = {
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

  override def toBytes(block: Block): Array[Byte] = {
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

  override def parseBytes(bytes: Array[ModifierTypeId]): Try[Block] = Try {

    val parentId = bytes.slice(0, Block.BlockIdLength)

    val Array(timestamp: Long, generatorBoxLen: Long) = (0 until 2).map {
      i => Longs.fromByteArray(bytes.slice(Block.BlockIdLength + i*Longs.BYTES, Block.BlockIdLength + (i + 1)*Longs.BYTES))
    }.toArray

    val version = bytes.slice(Block.BlockIdLength + 2*Longs.BYTES, Block.BlockIdLength + 2*Longs.BYTES + 1).head

    var numBytesRead = Block.BlockIdLength + Longs.BYTES*2 + 1

    val generatorBox = BoxSerializer.parseBytes(bytes.slice(numBytesRead, numBytesRead + generatorBoxLen.toInt)).get.asInstanceOf[ArbitBox]

    val inflation = bytes.slice(numBytesRead + generatorBoxLen.toInt, numBytesRead + generatorBoxLen.toInt + Longs.BYTES)

    val signature = Signature25519(bytes.slice(numBytesRead + generatorBoxLen.toInt + Longs.BYTES,
      numBytesRead + generatorBoxLen.toInt + Longs.BYTES + Signature25519.SignatureSize))

    numBytesRead += generatorBoxLen.toInt + Signature25519.SignatureSize + Longs.BYTES

    val numTxExpected = Ints.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Ints.BYTES))
    numBytesRead += Ints.BYTES

    require(numTxExpected >= 0)

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

        require(bytesToGrab >= 0)

        if (b.length - Ints.BYTES < bytesToGrab) {
          None // we're done because we can't grab the number of bytes required
        } else {
          val thisTx: Array[Byte] = b.slice(Ints.BYTES, Ints.BYTES + bytesToGrab)
          Some((b.slice(Ints.BYTES + bytesToGrab, b.length), thisTx))
        }
    }.ensuring(_.length == numTxExpected)

    val tx: Seq[BifrostTransaction] = txByteSeq.map(tx => BifrostTransactionCompanion.parseBytes(tx).get)

    Block(parentId, timestamp, generatorBox, signature, tx, Longs.fromByteArray(inflation), version)
  }


  def parseBytes2xAndBefore(bytes: Array[ModifierTypeId]): Try[Block] = Try {
    val parentId = bytes.slice(0, Block.BlockIdLength)
    val Array(timestamp: Long, generatorBoxLen: Long) = (0 until 2).map {
      i => Longs.fromByteArray(bytes.slice(Block.BlockIdLength + i * Longs.BYTES, Block.BlockIdLength + (i + 1) * Longs.BYTES))
    }.toArray

    val version = bytes.slice(Block.BlockIdLength + 2*Longs.BYTES, Block.BlockIdLength + 2*Longs.BYTES + 1).head

    var numBytesRead = Block.BlockIdLength + Longs.BYTES * 2 + 1

    val generatorBox = BoxSerializer.parseBytes(bytes.slice(numBytesRead, numBytesRead + generatorBoxLen.toInt)).get.asInstanceOf[ArbitBox]
    val signature = Signature25519(bytes.slice(numBytesRead + generatorBoxLen.toInt, numBytesRead + generatorBoxLen.toInt + Signature25519.SignatureSize))

    numBytesRead += generatorBoxLen.toInt + Signature25519.SignatureSize

    val numTxExpected = Ints.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Ints.BYTES))
    numBytesRead += Ints.BYTES

    require(numTxExpected >= 0)

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

        require(bytesToGrab >= 0)

        if (b.length - Ints.BYTES < bytesToGrab) {
          None // we're done because we can't grab the number of bytes required
        } else {
          val thisTx: Array[Byte] = b.slice(Ints.BYTES, Ints.BYTES + bytesToGrab)
          Some((b.slice(Ints.BYTES + bytesToGrab, b.length), thisTx))
        }
    }.ensuring(_.length == numTxExpected)

    val tx: Seq[BifrostTransaction] = txByteSeq.map(tx => BifrostTransactionCompanion.parseBytes(tx).get)

    Block(parentId, timestamp, generatorBox, signature, tx, protocolVersion = version)
  }
}
