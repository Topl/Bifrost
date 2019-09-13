package bifrost.transaction.serialization

import bifrost.serialization.Serializer
import bifrost.transaction.bifrostTransaction.BifrostTransaction.Nonce
import bifrost.transaction.bifrostTransaction._
import bifrost.transaction.box.proposition.{Constants25519, PublicKey25519Proposition}
import bifrost.transaction.proof.Signature25519
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.iohk.iodb.ByteArrayWrapper
import scorex.crypto.signatures.Curve25519

import scala.util.Try

object ProgramTransactionCompanion extends Serializer[ProgramTransaction] {

  val typeBytes: Array[Byte] = "ProgramTransaction".getBytes

  val prefixBytes: Array[Byte] = Ints.toByteArray(typeBytes.length) ++ typeBytes

  override def toBytes(m: ProgramTransaction): Array[Byte] = {
    prefixBytes ++
      (m match {
        case cc: ProgramCreation => ProgramCreationCompanion.toChildBytes(cc)
        case cme: ProgramMethodExecution => ProgramMethodExecutionCompanion.toChildBytes(cme)
      })
  }

  override def parseBytes(bytes: Array[Byte]): Try[ProgramTransaction] = Try {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))

    /* Grab the rest of the bytes, which should begin similarly (with sub-type) */
    val newBytes = bytes.slice(Ints.BYTES + typeLength, bytes.length)

    val newTypeLength = Ints.fromByteArray(newBytes.take(Ints.BYTES))
    val newTypeStr = new String(newBytes.slice(Ints.BYTES, Ints.BYTES + newTypeLength))

    newTypeStr match {
      case "ProgramCreation" => ProgramCreationCompanion.parseBytes(newBytes).get
      case "ProgramMethodExecution" => ProgramMethodExecutionCompanion.parseBytes(newBytes).get
    }
  }

  def commonToBytes(m: ProgramTransaction): Array[Byte] = {
    Bytes.concat(
      Longs.toByteArray(m.timestamp),
      m.owner.pubKeyBytes,
      m.signatures.head._2.bytes,
      Ints.toByteArray(m.preFeeBoxes.head._2.length),
      m.preFeeBoxes.head._2.foldLeft(Array[Byte]())((a,b) => a ++ Longs.toByteArray(b._1) ++ Longs.toByteArray(b._2)),
      Longs.toByteArray(m.fees.head._2)
    )
  }

  //noinspection ScalaStyle
  def commonParseBytes(bytes: Array[Byte]): (
      PublicKey25519Proposition,
      Map[PublicKey25519Proposition, Signature25519],
      Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
      Map[PublicKey25519Proposition, Long],
      Long
    ) = {

    var numReadBytes = 0

    val timestamp: Long = Longs.fromByteArray(bytes.slice(0, Longs.BYTES))

    numReadBytes += Longs.BYTES

    val owner = PublicKey25519Proposition(bytes.slice(numReadBytes, numReadBytes + Constants25519.PubKeyLength))

    numReadBytes += Constants25519.PubKeyLength

    val signatures: Map[PublicKey25519Proposition, Signature25519] = {
      val sig = Signature25519(bytes.slice(numReadBytes, numReadBytes + Curve25519.SignatureLength))
      Map(owner -> sig)
    }

    numReadBytes += Curve25519.SignatureLength

    val feePreBoxesLength = Ints.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Ints.BYTES))

    numReadBytes += Ints.BYTES

    val preBoxes: IndexedSeq[(Nonce, Long)] = (0 until feePreBoxesLength).map { _ =>
      val nonce: Nonce = Longs.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Longs.BYTES))
      numReadBytes += Longs.BYTES
      val amount = Longs.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Longs.BYTES))
      numReadBytes += Longs.BYTES

      nonce -> amount
    }

    val feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]] = Map(owner -> preBoxes)

    val fees: Map[PublicKey25519Proposition, Long] = Map(owner -> Longs.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Longs.BYTES)))

    (owner, signatures, feePreBoxes, fees, timestamp)
  }
}
