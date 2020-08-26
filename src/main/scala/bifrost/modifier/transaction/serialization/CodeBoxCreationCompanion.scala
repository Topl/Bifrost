package bifrost.modifier.transaction.serialization

import bifrost.crypto.Signature25519
import bifrost.modifier.box.proposition.{Constants25519, PublicKey25519Proposition}
import bifrost.modifier.transaction.bifrostTransaction.CodeCreation
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import com.google.common.primitives.{Bytes, Ints, Longs}
import scorex.crypto.signatures.Curve25519

import scala.util.Try

object CodeBoxCreationCompanion extends BifrostSerializer[CodeCreation]{

  override def toBytes(obj: CodeCreation): Array[Byte] = {

    val typeBytes = "CodeCreation".getBytes
    Bytes.concat(
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      obj.to.bytes,
      obj.signature.bytes,
      Ints.toByteArray(obj.code.getBytes.length),
      obj.code.getBytes,
      Longs.toByteArray(obj.fee),
      Longs.toByteArray(obj.timestamp),
      Ints.toByteArray(obj.data.getBytes.length),
      obj.data.getBytes
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[CodeCreation] = Try {

    val typeLen: Int = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr: String = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLen))

    require(typeStr == "CodeCreation")

    var numReadBytes = Ints.BYTES + typeLen

    val to: PublicKey25519Proposition = PublicKey25519Proposition(
      bytes.slice(numReadBytes, numReadBytes + Constants25519.PubKeyLength))

    numReadBytes += Constants25519.PubKeyLength

    val signature: Signature25519 = Signature25519(
      bytes.slice(numReadBytes, numReadBytes + Curve25519.SignatureLength))

    numReadBytes += Curve25519.SignatureLength

    val codeLen: Int = Ints.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Ints.BYTES))
    val code: String = new String(bytes.slice(numReadBytes + Ints.BYTES, numReadBytes + Ints.BYTES + codeLen))

    numReadBytes += Ints.BYTES + codeLen

    val Array(fee: Long, timestamp: Long) = (0 until 2).map { i =>
      Longs.fromByteArray(bytes.slice(numReadBytes + i * Longs.BYTES, numReadBytes + (i + 1) * Longs.BYTES))
    }.toArray

    numReadBytes += 2 * Longs.BYTES

    val dataLen: Int = Ints.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Ints.BYTES))
    val data: String = new String(bytes.slice(numReadBytes + Ints.BYTES, numReadBytes + Ints.BYTES + dataLen))

    CodeCreation(to, signature, code, fee, timestamp, data)
  }

  override def parse(r: Reader): CodeCreation = ???

  override def serialize(obj: CodeCreation, w: Writer): Unit = ???
}
