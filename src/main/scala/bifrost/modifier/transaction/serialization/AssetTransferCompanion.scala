package bifrost.modifier.transaction.serialization

import bifrost.crypto.{Signature25519, Signature25519Serializer}
import bifrost.modifier.box.proposition.{Constants25519, PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import bifrost.modifier.transaction.bifrostTransaction.AssetTransfer
import bifrost.modifier.transaction.bifrostTransaction.Transaction.Nonce
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import bifrost.utils.Extensions._
import com.google.common.primitives.Ints

import scala.util.Try

object AssetTransferCompanion extends BifrostSerializer[AssetTransfer] with TransferSerializer {

  override def serialize(obj: AssetTransfer, w: Writer): Unit = {
    /* from: IndexedSeq[(PublicKey25519Proposition, Nonce)] */
    w.putUInt(obj.from.length)
    obj.from.foreach { case (prop, nonce) =>
      PublicKey25519PropositionSerializer.serialize(prop, w)
      w.putLong(nonce)
    }

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

    /* fee: Long */
    w.putULong(obj.fee)

    /* timestamp: Long */
    w.putULong(obj.timestamp)

    /* data: String */
    w.putIntString(obj.data)

    /* issuer: PublicKey25519Proposition */
    PublicKey25519PropositionSerializer.serialize(obj.issuer, w)

    /* assetCode: String */
    w.putIntString(obj.assetCode)
  }

  override def parse(r: Reader): AssetTransfer = {
    val fromLength: Int = r.getUInt().toIntExact
    val from: IndexedSeq[(PublicKey25519Proposition, Nonce)] = (0 until fromLength).map { _ =>
      val prop = PublicKey25519PropositionSerializer.parse(r)
      val nonce = r.getLong()
      prop -> nonce
    }

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

    val fee: Long = r.getULong()
    val timestamp: Long = r.getULong()
    val data: String = r.getIntString()
    val issuer: PublicKey25519Proposition = PublicKey25519PropositionSerializer.parse(r)
    val assetCode: String = r.getIntString()

    AssetTransfer(from, to, signatures, issuer, assetCode, fee, timestamp, data)
  }

// TODO: Jing - remove
//
//  override def toBytes(at: AssetTransfer): Array[Byte] = {
//    TransferTransactionCompanion.prefixBytes ++ toChildBytes(at)
//  }
//
//  def toChildBytes(at: AssetTransfer): Array[Byte] = {
//    transferToBytes(at, "AssetTransfer") ++
//      at.issuer.pubKeyBytes ++
//      at.assetCode.getBytes ++
//      Ints.toByteArray(at.assetCode.getBytes.length)++
//      at.data.getBytes++
//      Ints.toByteArray(at.data.getBytes.length)
//  }
//
//  override def parseBytes(bytes: Array[Byte]): Try[AssetTransfer] = Try {
//    val params = parametersParseBytes(bytes)
//
//    val dataLen: Int = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES, bytes.length))
//    val data: String = new String(
//      bytes.slice(bytes.length - Ints.BYTES - dataLen, bytes.length - Ints.BYTES)
//    )
//
//    val assetCodeLen: Int = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES - dataLen - Ints.BYTES, bytes.length - Ints.BYTES - dataLen))
//    val assetCode: String = new String(
//      bytes.slice(bytes.length - Ints.BYTES - assetCodeLen - Ints.BYTES - dataLen, bytes.length - Ints.BYTES - dataLen - Ints.BYTES)
//    )
//
//    val issuer: PublicKey25519Proposition = PublicKey25519Proposition(
//      bytes.slice(bytes.length - Ints.BYTES - assetCodeLen - Ints.BYTES - dataLen - Constants25519.PubKeyLength,
//        bytes.length - Ints.BYTES - assetCodeLen - Ints.BYTES - dataLen)
//    )
//
//    AssetTransfer(params._1, params._2, params._3, issuer, assetCode, params._4, params._5, data)
//  }
}
