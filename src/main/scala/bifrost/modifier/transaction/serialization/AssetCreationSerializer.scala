package bifrost.modifier.transaction.serialization

import bifrost.crypto.Signature25519
import bifrost.crypto.serialization.Signature25519Serializer
import bifrost.modifier.box.proposition.{Constants25519, PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import bifrost.modifier.transaction.bifrostTransaction.AssetCreation
import bifrost.modifier.transaction.bifrostTransaction.Transaction.Nonce
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import bifrost.utils.Extensions._
import com.google.common.primitives.{Bytes, Ints, Longs}
import scorex.crypto.signatures.Curve25519

import scala.util.Try

object AssetCreationSerializer extends BifrostSerializer[AssetCreation] {

  override def serialize(obj: AssetCreation, w: Writer): Unit = {
    /* to: IndexedSeq[(PublicKey25519Proposition, Long)] */
    w.putUInt(obj.to.length)
    obj.to.foreach { case (prop, value) =>
      PublicKey25519PropositionSerializer.serialize(prop, w)
      w.putULong(value)
    }

    /* signatures: Map[PublicKey25519Proposition, Signature25519] */
    w.putUInt(obj.signatures.size)
    obj.signatures.foreach { case (prop, sig) =>
      PublicKey25519PropositionSerializer.serialize(prop, w)
      Signature25519Serializer.serialize(sig, w)
    }

    /* assetCode: String */
    w.putIntString(obj.assetCode)

    /* issuer: PublicKey25519Proposition */
    PublicKey25519PropositionSerializer.serialize(obj.issuer, w)

    /* fee: Long */
    w.putULong(obj.fee)

    /* timestamp: Long */
    w.putULong(obj.timestamp)

    /* data: String */
    w.putIntString(obj.data)
  }

  override def parse(r: Reader): AssetCreation = {
    val toLength: Int = r.getUInt().toIntExact
    val to: IndexedSeq[(PublicKey25519Proposition, Long)] = (0 until toLength).map { _ =>
      val prop = PublicKey25519PropositionSerializer.parse(r)
      val value = r.getULong()
      prop -> value
    }

    val signaturesLength: Int = r.getUInt().toIntExact
    val signatures: Map[PublicKey25519Proposition, Signature25519] = (0 until signaturesLength).map { _ =>
      val prop = PublicKey25519PropositionSerializer.parse(r)
      val sig = Signature25519Serializer.parse(r)
      prop -> sig
    }.toMap

    val assetCode: String = r.getIntString()
    val issuer: PublicKey25519Proposition = PublicKey25519PropositionSerializer.parse(r)
    val fee: Long = r.getULong()
    val timestamp: Long = r.getULong()
    val data: String = r.getIntString()

    AssetCreation(to, signatures, assetCode, issuer, fee, timestamp, data)
  }

// TODO: Jing - remove
//
//  override def toBytes(ac: AssetCreation): Array[Byte] = {
//    val typeBytes = "AssetCreation".getBytes
//    Bytes.concat(
//      Ints.toByteArray(typeBytes.length),
//      typeBytes,
//      Longs.toByteArray(ac.fee),
//      Longs.toByteArray(ac.timestamp),
//      Ints.toByteArray(ac.signatures.size),
//      Ints.toByteArray(ac.to.size),
//      Ints.toByteArray(ac.assetCode.getBytes.length),
//      ac.assetCode.getBytes,
//      ac.issuer.pubKeyBytes,
//      ac.signatures.foldLeft(Array[Byte]())((a, b) => a ++ b._1.bytes ++ b._2.bytes),
//      ac.to.foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2)),
//      ac.data.getBytes,
//      Ints.toByteArray(ac.data.getBytes.length)
//    )
//  }
//
//  //noinspection ScalaStyle
//  override def parseBytes(bytes: Array[Byte]): Try[AssetCreation] = Try {
//    val dataLen: Int = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES, bytes.length))
//    val data: String = new String(
//      bytes.slice(bytes.length - Ints.BYTES - dataLen, bytes.length - Ints.BYTES)
//    )
//    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
//    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))
//
//    require(typeStr == "AssetCreation")
//
//    var numReadBytes = Ints.BYTES + typeLength
//    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)
//
//    val Array(fee: Long, timestamp: Long) = (0 until 2).map { i =>
//      Longs.fromByteArray(bytesWithoutType.slice(i * Longs.BYTES, (i + 1) * Longs.BYTES))
//    }.toArray
//    numReadBytes = 2 * Longs.BYTES
//
//    val sigLength = Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES))
//    numReadBytes += Ints.BYTES
//
//    val toLength = Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES))
//    numReadBytes += Ints.BYTES
//
//    val assetCodeLen: Int = Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES))
//    numReadBytes += Ints.BYTES
//
//    val assetCode: String = new String(
//      bytesWithoutType.slice(numReadBytes, numReadBytes + assetCodeLen)
//    )
//    numReadBytes += assetCodeLen
//
//    val issuer = PublicKey25519Proposition(bytesWithoutType.slice(numReadBytes,
//      numReadBytes + Constants25519.PubKeyLength))
//    numReadBytes += Constants25519.PubKeyLength
//
//    val signatures = (0 until sigLength) map { i =>
//      (PublicKey25519Proposition(bytesWithoutType.slice(numReadBytes + i * (Curve25519.KeyLength + Curve25519.SignatureLength),
//        numReadBytes + i * (Curve25519.KeyLength + Curve25519.SignatureLength) + Curve25519.KeyLength)),
//        Signature25519(bytesWithoutType.slice(numReadBytes + i * (Curve25519.KeyLength + Curve25519.SignatureLength) + Curve25519.KeyLength,
//          numReadBytes + (i+1) * (Curve25519.KeyLength + Curve25519.SignatureLength))))
//    }
//    numReadBytes += sigLength * (Curve25519.SignatureLength + Curve25519.KeyLength)
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
//    AssetCreation(to, signatures.toMap, assetCode, issuer, fee, timestamp, data)
//  }
}
