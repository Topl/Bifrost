package bifrost.transaction.serialization

import bifrost.serialization.Serializer
import bifrost.transaction.bifrostTransaction.ProgramTransfer
import bifrost.transaction.box.{ExecutionBox, ExecutionBoxSerializer}
import bifrost.transaction.box.proposition.{Constants25519, PublicKey25519Proposition}
import bifrost.transaction.proof.Signature25519
import com.google.common.primitives.{Bytes, Ints, Longs}
import scorex.crypto.signatures.Curve25519

import scala.util.Try

object ProgramTransferCompanion extends Serializer[ProgramTransfer]{

  override def toBytes(obj: ProgramTransfer): Array[Byte] = {

    val typeBytes = "ProgramTransfer".getBytes

    Bytes.concat(
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      obj.from.pubKeyBytes,
      obj.to.pubKeyBytes,
      obj.signature.signature,
      Ints.toByteArray(obj.executionBox.bytes.length),
      ExecutionBoxSerializer.toBytes(obj.executionBox),
      Longs.toByteArray(obj.fee),
      Longs.toByteArray(obj.timestamp),
      Ints.toByteArray(obj.data.length),
      obj.data.getBytes
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[ProgramTransfer] = Try {

    val typeLen: Int = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr: String = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLen))

    var numReadBytes: Int = Ints.BYTES + typeLen

    val from: PublicKey25519Proposition = PublicKey25519Proposition(
        bytes.slice(numReadBytes, numReadBytes + Constants25519.PubKeyLength))

    numReadBytes += Constants25519.PubKeyLength

    val to: PublicKey25519Proposition = PublicKey25519Proposition(
      bytes.slice(numReadBytes, numReadBytes + Constants25519.PubKeyLength))

    numReadBytes += Constants25519.PubKeyLength

    val signature: Signature25519 = Signature25519(bytes.slice(numReadBytes, numReadBytes + Curve25519.SignatureLength))

    numReadBytes += Curve25519.SignatureLength

    val executionBoxlen: Int = Ints.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Ints.BYTES))
    val executionBox: ExecutionBox = ExecutionBoxSerializer.parseBytes(
      bytes.slice(numReadBytes + Ints.BYTES, numReadBytes + Ints.BYTES + executionBoxlen)).get

    numReadBytes += Ints.BYTES + executionBoxlen

    val (fee, timestamp): (Long, Long) = {
      (Longs.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Longs.BYTES)),
      Longs.fromByteArray(bytes.slice(numReadBytes + Longs.BYTES, numReadBytes + Longs.BYTES * 2)))
    }

    numReadBytes += Longs.BYTES * 2

    val dataLen: Int = Ints.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Ints.BYTES))
    val data: String = new String(bytes.slice(numReadBytes + Ints.BYTES, numReadBytes + Ints.BYTES + dataLen))

    ProgramTransfer(from, to, signature, executionBox, fee, timestamp, data)
  }
}
