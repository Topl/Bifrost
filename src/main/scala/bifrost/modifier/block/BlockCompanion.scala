package bifrost.modifier.block

import bifrost.crypto.{Signature25519, Signature25519Serializer}
import bifrost.modifier.ModifierId
import bifrost.modifier.box.{ArbitBox, BoxSerializer}
import bifrost.modifier.transaction.bifrostTransaction.Transaction
import bifrost.modifier.transaction.serialization.TransactionCompanion
import bifrost.utils.{bytesToId, idToBytes}
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import bifrost.utils.Extensions._
import com.google.common.primitives.{Bytes, Ints, Longs}

import scala.annotation.tailrec
import scala.util.Try

object BlockCompanion extends BifrostSerializer[Block] {

  override def serialize(block: Block, w: Writer): Unit = {
    /* parentId: ModifierId */
    w.putBytes(block.parentId.hashBytes)

    /* timestamp: Long */
    w.putULong(block.timestamp)

    /* version: Byte */
    w.put(block.version)

    /* generatorBox: ArbitBox */
    BoxSerializer.serialize(block.forgerBox, w)

    /* inflation: Long */
    w.putLong(block.inflation)

    /* signature: Signature25519 */
    Signature25519Serializer.serialize(block.signature, w)

    /* txsLength: Int */
    w.putUInt(block.txs.length)

    /* txs: Seq[Transaction] */
    block.txs.foreach(tx => TransactionCompanion.serialize(tx, w))
  }

  override def parse(r: Reader): Block = {
    // The order of the getByte, getLong... calls should not be changed

    // TODO: Jing - maybe we could check that the size of bytes to read in reader is less or equal to the max size of a block

    // TODO: Jing - here using ModifierId instead of bytesToId, we could get rid of bytesToId soon
    val parentId: ModifierId = ModifierId(r.getBytes(Block.blockIdLength))

    val timestamp: Long = r.getULong()

    // TODO: Jing - scorex uses toIntExact to make sure the Long does not exceed the length of an Int
    // TODO: Jing - generatorBoxLen is probably not useful anymore
    //    val generatorBoxLen: Int = r.getUInt().toIntExact

    // Version should be used in the future to determine if we need additional procedures in parsing
    val version: Byte = r.getByte()

    // TODO: Jing - BoxSerializer.parseBytes: should switch to using .getBytes later
    val generatorBox: ArbitBox = BoxSerializer.parse(r).asInstanceOf[ArbitBox]

    // TODO: Jing - Can inflation be negative?
    val inflation: Long = r.getLong()

    val signature: Signature25519 = Signature25519Serializer.parse(r)

    val txsLength: Int = r.getUInt().toIntExact
    /* implement parse in TransactionCompanion and its specific transactionCompanions 3 layer of companions */
    val txs: Seq[Transaction] = (0 until txsLength).map(_ => TransactionCompanion.parse(r))

    Block(parentId, timestamp, generatorBox, signature, txs, inflation, version)
  }

//  TODO: Jing - remove
//
//  def commonMessage(block: Block): Array[Byte] = {
//    val numTx = Ints.toByteArray(block.txs.length)
//    val generatorBoxBytes = BoxSerializer.toBytes(block.forgerBox)
//
//    Bytes.concat(
//      idToBytes(block.parentId),
//      Longs.toByteArray(block.timestamp),
//      Longs.toByteArray(generatorBoxBytes.length),
//      Array(block.version),
//      generatorBoxBytes,
//      Longs.toByteArray(block.inflation),
//      block.signature.signature,
//      numTx // writes number of transactions, then adds <tx as bytes>| <number of bytes for tx as bytes> for each tx
//    )
//  }
//
//  def commonMessage2xAndBefore(block: Block): Array[Byte] = {
//    val numTx = Ints.toByteArray(block.txs.length)
//    val generatorBoxBytes = BoxSerializer.toBytes(block.forgerBox)
//
//    Bytes.concat(
//      idToBytes(block.parentId),
//      Longs.toByteArray(block.timestamp),
//      Longs.toByteArray(generatorBoxBytes.length),
//      Array(block.version),
//      generatorBoxBytes,
//      block.signature.signature,
//      numTx // writes number of transactions, then adds <tx as bytes>| <number of bytes for tx as bytes> for each tx
//    )
//  }
//
//  def messageToSign(block: Block): Array[Byte] = {
//    val commonBytes: Array[Byte] = {
//      block.version match {
//        case 0 => commonMessage2xAndBefore(block)
//        case _ => commonMessage(block)
//      }
//    }
//    //noinspection ScalaStyle
//    if (block.parentId.hashBytes sameElements Array.fill(32)(1: Byte)) {
//      commonBytes ++ block.txs.foldLeft(Array[Byte]())((bytes, tx) => bytes ++ Ints.toByteArray(TransactionCompanion.toBytes(tx).length) ++ tx.messageToSign)
//    } else {
//      commonBytes ++ block.txs.foldLeft(Array[Byte]())((bytes, tx) => bytes ++ Ints.toByteArray(TransactionCompanion.toBytes(tx).length) ++ TransactionCompanion.toBytes(tx))
//    }
//  }
//  override def toBytes(block: Block): Array[Byte] = {
//    block.version match {
//      case 0 =>
//        commonMessage2xAndBefore(block) ++ block.txs.foldLeft(Array[Byte]())((bytes, tx) =>
//          bytes ++
//            Ints.toByteArray(TransactionCompanion.toBytes(tx).length) ++
//            TransactionCompanion.toBytes(tx))
//      case _ =>
//        commonMessage(block) ++ block.txs.foldLeft(Array[Byte]())((bytes, tx) =>
//          bytes ++
//            Ints.toByteArray(TransactionCompanion.toBytes(tx).length) ++
//            TransactionCompanion.toBytes(tx))
//    }
//  }
//
//
//  override def parseBytes(bytes: Array[Byte]): Try[Block] = Try {
//
//    val parentId = bytesToId(bytes.slice(0, Block.blockIdLength))
//
//    val Array(timestamp: Long, generatorBoxLen: Long) = (0 until 2).map {
//      i => Longs.fromByteArray(bytes.slice(Block.blockIdLength + i*Longs.BYTES, Block.blockIdLength + (i + 1)*Longs.BYTES))
//    }.toArray
//
//    val version = bytes.slice(Block.blockIdLength + 2*Longs.BYTES, Block.blockIdLength + 2*Longs.BYTES + 1).head
//
//    var numBytesRead = Block.blockIdLength + Longs.BYTES*2 + 1
//
//    val generatorBox = BoxSerializer.parseBytes(bytes.slice(numBytesRead, numBytesRead + generatorBoxLen.toInt)).get.asInstanceOf[ArbitBox]
//
//    val inflation = bytes.slice(numBytesRead + generatorBoxLen.toInt, numBytesRead + generatorBoxLen.toInt + Longs.BYTES)
//
//    val signature = Signature25519(bytes.slice(numBytesRead + generatorBoxLen.toInt + Longs.BYTES,
//      numBytesRead + generatorBoxLen.toInt + Longs.BYTES + Signature25519.SignatureSize))
//
//    numBytesRead += generatorBoxLen.toInt + Longs.BYTES + Signature25519.SignatureSize
//
//    val numTxExpected = Ints.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Ints.BYTES))
//    numBytesRead += Ints.BYTES
//
//    require(numTxExpected >= 0)
//
//    def unfoldLeft[A,B](seed: B)(f: B => Option[(B, A)]): Seq[A] = {
//      @tailrec
//      def loop(seed: B)(ls: Seq[A]): Seq[A] = f(seed) match {
//        case Some((b, a)) => loop(b)(a +: ls)
//        case None => ls
//      }
//      loop(seed)(Nil)
//    }.reverse
//
//    val txBytes: Array[Byte] = bytes.slice(numBytesRead, bytes.length)
//
//    val txByteSeq: Seq[Array[Byte]] = unfoldLeft(txBytes) {
//      case b if b.length < Ints.BYTES => None
//      case b =>
//        val bytesToGrab = Ints.fromByteArray(b.take(Ints.BYTES))
//
//        require(bytesToGrab >= 0)
//
//        if (b.length - Ints.BYTES < bytesToGrab) {
//          None // we're done because we can't grab the number of bytes required
//        } else {
//          val thisTx: Array[Byte] = b.slice(Ints.BYTES, Ints.BYTES + bytesToGrab)
//          Some((b.slice(Ints.BYTES + bytesToGrab, b.length), thisTx))
//        }
//    }.ensuring(_.length == numTxExpected)
//
//    val tx: Seq[Transaction] = txByteSeq.map(tx => TransactionCompanion.parseBytes(tx).get)
//
//    Block(parentId, timestamp, generatorBox, signature, tx, Longs.fromByteArray(inflation), version)
//  }
//
//
//  def parseBytes2xAndBefore(bytes: Array[Byte]): Try[Block] = Try {
//    val parentId = bytesToId(bytes.slice(0, Block.blockIdLength))
//
//    val Array(timestamp: Long, generatorBoxLen: Long) = (0 until 2).map {
//      i => Longs.fromByteArray(bytes.slice(Block.blockIdLength + i * Longs.BYTES, Block.blockIdLength + (i + 1) * Longs.BYTES))
//    }.toArray
//
//    val version = bytes.slice(Block.blockIdLength + 2*Longs.BYTES, Block.blockIdLength + 2*Longs.BYTES + 1).head
//
//    var numBytesRead = Block.blockIdLength + Longs.BYTES * 2 + 1
//
//    val generatorBox = BoxSerializer.parseBytes(bytes.slice(numBytesRead, numBytesRead + generatorBoxLen.toInt)).get.asInstanceOf[ArbitBox]
//    val signature = Signature25519(bytes.slice(numBytesRead + generatorBoxLen.toInt, numBytesRead + generatorBoxLen.toInt + Signature25519.SignatureSize))
//
//    numBytesRead += generatorBoxLen.toInt + Signature25519.SignatureSize
//
//    val numTxExpected = Ints.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Ints.BYTES))
//    numBytesRead += Ints.BYTES
//
//    require(numTxExpected >= 0)
//
//    def unfoldLeft[A,B](seed: B)(f: B => Option[(B, A)]): Seq[A] = {
//      @tailrec
//      def loop(seed: B)(ls: Seq[A]): Seq[A] = f(seed) match {
//        case Some((b, a)) => loop(b)(a +: ls)
//        case None => ls
//      }
//      loop(seed)(Nil)
//    }.reverse
//
//    val txBytes: Array[Byte] = bytes.slice(numBytesRead, bytes.length)
//
//    val txByteSeq: Seq[Array[Byte]] = unfoldLeft(txBytes) {
//      case b if b.length < Ints.BYTES => None
//      case b =>
//        val bytesToGrab = Ints.fromByteArray(b.take(Ints.BYTES))
//
//        require(bytesToGrab >= 0)
//
//        if (b.length - Ints.BYTES < bytesToGrab) {
//          None // we're done because we can't grab the number of bytes required
//        } else {
//          val thisTx: Array[Byte] = b.slice(Ints.BYTES, Ints.BYTES + bytesToGrab)
//          Some((b.slice(Ints.BYTES + bytesToGrab, b.length), thisTx))
//        }
//    }.ensuring(_.length == numTxExpected)
//
//    val tx: Seq[Transaction] = txByteSeq.map(tx => TransactionCompanion.parseBytes(tx).get)
//
//    Block(parentId, timestamp, generatorBox, signature, tx, protocolVersion = version)
//  }
}
