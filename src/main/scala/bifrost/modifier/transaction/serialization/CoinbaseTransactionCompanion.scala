package bifrost.modifier.transaction.serialization

import bifrost.crypto.{Signature25519, Signature25519Serializer}
import bifrost.modifier.box.proposition.{PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import bifrost.modifier.transaction.bifrostTransaction.CoinbaseTransaction
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import bifrost.utils.Extensions._
import com.google.common.primitives.{Bytes, Ints, Longs}
import scorex.crypto.signatures.Curve25519

import scala.util.Try

object CoinbaseTransactionCompanion extends BifrostSerializer[CoinbaseTransaction] {

  override def serialize(obj: CoinbaseTransaction, w: Writer): Unit = {
    /* to: IndexedSeq[(PublicKey25519Proposition, Long)] */
    w.putUInt(obj.to.length)
    obj.to.foreach { case (prop, value) =>
      PublicKey25519PropositionSerializer.serialize(prop, w)
      w.putULong(value)
    }

    /* signatures: IndexedSeq[Signature25519] */
    w.putUInt(obj.signatures.size)
    obj.signatures.foreach(sig => Signature25519Serializer.serialize(sig, w))

    /* timestamp: Long */
    w.putULong(obj.timestamp)

    /* blockID: Array[Byte] */
    w.putUInt(obj.blockID.length)
    w.putBytes(obj.blockID)
  }

  override def parse(r: Reader): CoinbaseTransaction = {
    val toLength: Int = r.getUInt().toIntExact
    val to: IndexedSeq[(PublicKey25519Proposition, Long)] = (0 until toLength).map { _ =>
      val prop = PublicKey25519PropositionSerializer.parse(r)
      val value = r.getULong()
      prop -> value
    }

    val signaturesLength: Int = r.getUInt().toIntExact
    val signatures: IndexedSeq[Signature25519] = (0 until signaturesLength).map(_ => Signature25519Serializer.parse(r))

    val timestamp: Long = r.getULong()

    val blockIDLength: Int = r.getUInt().toIntExact
    val blockID: Array[Byte] = r.getBytes(blockIDLength)

    CoinbaseTransaction(to, signatures, timestamp, blockID)
  }

// TODO: Jing - remove
//
//  override def toBytes(obj: CoinbaseTransaction): Array[Byte] = {
//    val typeBytes = "CoinbaseTransaction".getBytes
//    Bytes.concat(
//      Ints.toByteArray(typeBytes.length),
//      typeBytes,
//      Longs.toByteArray(obj.fee),
//      Longs.toByteArray(obj.timestamp),
//      obj.blockID,
//      Ints.toByteArray(obj.signatures.length),
//      Ints.toByteArray(obj.to.size),
//      obj.signatures.foldLeft(Array[Byte]())((a, b) => a ++ b.bytes),
//      obj.to.foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2))
//    )
//  }
//
//  //noinspection ScalaStyle
//  override def parseBytes(bytes: Array[Byte]): Try[CoinbaseTransaction] = Try {
//    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
//    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))
//
//    require(typeStr == "CoinbaseTransaction")
//
//    var numReadBytes = Ints.BYTES + typeLength
//    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)
//
//    val Array(fee: Long, timestamp: Long) = (0 until 2).map { i =>
//      Longs.fromByteArray(bytesWithoutType.slice(i * Longs.BYTES, (i + 1) * Longs.BYTES))
//    }.toArray
//
//    require(fee == 0L)
//
//    numReadBytes = 2 * Longs.BYTES
//
//    val blockID = bytesWithoutType.slice(numReadBytes, numReadBytes + 32)
//
//    numReadBytes += 32
//
//    val sigLength = Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES))
//
//    numReadBytes += Ints.BYTES
//
//    val toLength = Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES))
//
//    numReadBytes += Ints.BYTES
//
//    val signatures = (0 until sigLength) map { i =>
//      Signature25519(bytesWithoutType.slice(numReadBytes + i * Curve25519.SignatureLength,
//        numReadBytes + (i + 1) * Curve25519.SignatureLength))
//    }
//
//    numReadBytes += sigLength * Curve25519.SignatureLength
//
//    val elementLength = Longs.BYTES + Curve25519.KeyLength
//
//    val to = (0 until toLength) map { i =>
//      val pk = bytesWithoutType.slice(numReadBytes + i * elementLength, numReadBytes + (i + 1) * elementLength - Longs.BYTES)
//      val v = Longs.fromByteArray(
//        bytesWithoutType.slice(numReadBytes + (i + 1) * elementLength - Longs.BYTES, numReadBytes + (i + 1) * elementLength)
//      )
//      (PublicKey25519Proposition(pk), v)
//    }
//
//
//    CoinbaseTransaction(to, signatures, timestamp, blockID)
//  }
}
