package bifrost.transaction.serialization

import bifrost.transaction.bifrostTransaction.TransferTransaction
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import com.google.common.primitives.{Bytes, Ints, Longs}
import scorex.crypto.signatures.Curve25519

trait TransferSerializer {
  def transferToBytes(tx: TransferTransaction, txType: String): Array[Byte] = {
    val typeBytes = txType.getBytes

    Bytes.concat(
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      Longs.toByteArray(tx.fee),
      Longs.toByteArray(tx.timestamp),
      Ints.toByteArray(tx.signatures.size),
      Ints.toByteArray(tx.from.length),
      Ints.toByteArray(tx.to.length),
      tx.signatures.foldLeft(Array[Byte]())((a, b) => a ++ b._1.bytes ++ b._2.bytes),
      tx.from.foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2)),
      tx.to.foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2))
    )
  }

  def parametersParseBytes(bytes: Array[Byte]): (IndexedSeq[(PublicKey25519Proposition, Long)],
    IndexedSeq[(PublicKey25519Proposition, Long)],
    Map[PublicKey25519Proposition, Signature25519], Long, Long) = {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))

    var numBytesRead = Ints.BYTES + typeLength

    val fee = Longs.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Longs.BYTES))
    val timestamp = Longs.fromByteArray(bytes.slice(numBytesRead + Longs.BYTES, numBytesRead + 2 * Longs.BYTES))
    val sigLength = Ints.fromByteArray(bytes.slice(numBytesRead + 2 * Longs.BYTES,
      numBytesRead + 2 * Longs.BYTES + Ints.BYTES))

    numBytesRead += 2 * Longs.BYTES + Ints.BYTES

    val fromLength = Ints.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Ints.BYTES))
    val toLength = Ints.fromByteArray(bytes.slice(numBytesRead + Ints.BYTES, numBytesRead + 2 * Ints.BYTES))

    numBytesRead += 2 * Ints.BYTES

    val signatures = (0 until sigLength) map { i =>
      (PublicKey25519Proposition(bytes.slice(numBytesRead + i * (Curve25519.KeyLength + Curve25519.SignatureLength),
        numBytesRead + i * (Curve25519.KeyLength + Curve25519.SignatureLength) + Curve25519.KeyLength)),
      Signature25519(bytes.slice(numBytesRead + i * (Curve25519.KeyLength + Curve25519.SignatureLength) + Curve25519.KeyLength,
        numBytesRead + (i+1) * (Curve25519.KeyLength + Curve25519.SignatureLength))))
    }

    numBytesRead += sigLength * (Curve25519.SignatureLength + Curve25519.KeyLength)

    val elementLength = Longs.BYTES + Curve25519.KeyLength

    val from = (0 until fromLength) map { i =>
      val pk = bytes.slice(numBytesRead + i * elementLength, numBytesRead + (i + 1) * elementLength - Longs.BYTES)
      val nonce = Longs.fromByteArray(
        bytes.slice(numBytesRead + (i + 1) * elementLength - Longs.BYTES, numBytesRead + (i + 1) * elementLength)
      )
      (PublicKey25519Proposition(pk), nonce)
    }

    numBytesRead += fromLength * elementLength
    val to = (0 until toLength) map { i =>
      val pk = bytes.slice(numBytesRead + i * elementLength, numBytesRead + (i + 1) * elementLength - Longs.BYTES)
      val v = Longs.fromByteArray(
        bytes.slice(numBytesRead + (i + 1) * elementLength - Longs.BYTES, numBytesRead + (i + 1) * elementLength)
      )
      (PublicKey25519Proposition(pk), v)
    }
    (from, to, signatures.toMap, fee, timestamp)
  }
}
