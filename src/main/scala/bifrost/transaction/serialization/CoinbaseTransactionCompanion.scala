package bifrost.transaction.serialization

import bifrost.serialization.Serializer
import bifrost.transaction.bifrostTransaction.CoinbaseTransaction
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import com.google.common.primitives.{Bytes, Ints, Longs}
import scorex.crypto.signatures.Curve25519

import scala.util.Try

object CoinbaseTransactionCompanion extends Serializer[CoinbaseTransaction] {
  override def toBytes(obj: CoinbaseTransaction): Array[Byte] = {
    val typeBytes = "CoinbaseTransaction".getBytes
    Bytes.concat(
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      Longs.toByteArray(obj.fee),
      Longs.toByteArray(obj.timestamp),
      obj.blockID,
      Ints.toByteArray(obj.signatures.length),
      Ints.toByteArray(obj.to.size),
      obj.signatures.foldLeft(Array[Byte]())((a, b) => a ++ b.bytes),
      obj.to.foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2))
    )
  }

  //noinspection ScalaStyle
  override def parseBytes(bytes: Array[Byte]): Try[CoinbaseTransaction] = Try {
    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))
    var numReadBytes = Ints.BYTES + typeLength
    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)

    val Array(fee: Long, timestamp: Long) = (0 until 2).map { i =>
      Longs.fromByteArray(bytesWithoutType.slice(i * Longs.BYTES, (i + 1) * Longs.BYTES))
    }.toArray

    numReadBytes = 2 * Longs.BYTES

    val blockID = bytesWithoutType.slice(numReadBytes, numReadBytes + 32)

    numReadBytes += 32

    val sigLength = Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES))

    numReadBytes += Ints.BYTES

    val toLength = Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES))

    numReadBytes += Ints.BYTES

    val signatures = (0 until sigLength) map { i =>
      Signature25519(bytesWithoutType.slice(numReadBytes + i * Curve25519.SignatureLength,
        numReadBytes + (i + 1) * Curve25519.SignatureLength))
    }

    numReadBytes += sigLength * Curve25519.SignatureLength

    val elementLength = Longs.BYTES + Curve25519.KeyLength

    val to = (0 until toLength) map { i =>
      val pk = bytesWithoutType.slice(numReadBytes + i * elementLength, numReadBytes + (i + 1) * elementLength - Longs.BYTES)
      val v = Longs.fromByteArray(
        bytesWithoutType.slice(numReadBytes + (i + 1) * elementLength - Longs.BYTES, numReadBytes + (i + 1) * elementLength)
      )
      (PublicKey25519Proposition(pk), v)
    }


    CoinbaseTransaction(to, signatures, timestamp, blockID)
  }
}
