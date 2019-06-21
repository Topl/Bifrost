package bifrost.transaction.serialization

import bifrost.serialization.Serializer
import bifrost.transaction.bifrostTransaction.BifrostTransaction.Nonce
import bifrost.transaction.bifrostTransaction.Role.Role
import bifrost.transaction._
import bifrost.transaction.bifrostTransaction._
import bifrost.transaction.box.proposition.{Constants25519, PublicKey25519Proposition}
import bifrost.transaction.proof.Signature25519
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.iohk.iodb.ByteArrayWrapper
import scorex.crypto.signatures.Curve25519

import scala.util.Try

object ProgramTransactionCompanion extends Serializer[ProgramTransaction] {

  val typeBytes = "ProgramTransaction".getBytes

  val prefixBytes = Ints.toByteArray(typeBytes.length) ++ typeBytes

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

    // Used to reduce overall size in the default case where publickeys are the same across multiple maps
    val keySeq = (m.signatures.keySet ++ m.fees.keySet ++ m.parties.keys).map(v => ByteArrayWrapper(v.pubKeyBytes))
      .toSeq.zipWithIndex
    val keyMapping: Map[ByteArrayWrapper, Int] = keySeq.toMap

    Bytes.concat(
      Longs.toByteArray(m.timestamp),
      Ints.toByteArray(m.signatures.size),
      Ints.toByteArray(m.parties.size),
      Ints.toByteArray(m.preFeeBoxes.size),
      Ints.toByteArray(m.fees.size),
      Ints.toByteArray(keyMapping.size),
      keySeq.foldLeft(Array[Byte]())((a, b) => a ++ b._1.data),
      m.parties.foldLeft(Array[Byte]())((a, b) => {
        a ++ Ints.toByteArray(keyMapping(ByteArrayWrapper(b._1.pubKeyBytes))) ++ (b._2 match {
          case Role.Producer => Ints.toByteArray(0)
          case Role.Investor => Ints.toByteArray(1)
          case Role.Hub => Ints.toByteArray(2)
        })
      }),
      m.signatures.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(keyMapping(ByteArrayWrapper(b._1
        .pubKeyBytes))) ++ b
        ._2.bytes),
      m.preFeeBoxes.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(keyMapping(ByteArrayWrapper(b._1
        .pubKeyBytes))) ++ Ints
        .toByteArray(b._2.length) ++
        b._2.foldLeft(Array[Byte]())((a, b) => a ++ Longs.toByteArray(b._1) ++ Longs.toByteArray(b._2))),
      m.fees.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(keyMapping(ByteArrayWrapper(b._1
        .pubKeyBytes))) ++ Longs
        .toByteArray(b._2))
    )
  }

  //noinspection ScalaStyle
  def commonParseBytes(bytes: Array[Byte]): (
      Map[PublicKey25519Proposition, Role],
      Map[PublicKey25519Proposition, Signature25519],
      Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
      Map[PublicKey25519Proposition, Long],
      Long
    ) = {

    var numReadBytes = 0

    val timestamp: Long = Longs.fromByteArray(bytes.slice(0, Longs.BYTES))

    numReadBytes += Longs.BYTES

    val Array(sigLength: Int,
    partiesLength: Int,
    feePreBoxLength: Int,
    feesLength: Int,
    keyMappingSize: Int) = (0 until 5).map { i =>
      Ints.fromByteArray(bytes.slice(numReadBytes + i * Ints.BYTES, numReadBytes + (i + 1) * Ints.BYTES))
    }.toArray

    numReadBytes += 5 * Ints.BYTES

    val keyMapping: Map[Int, PublicKey25519Proposition] = (0 until keyMappingSize).map { i =>
      i -> PublicKey25519Proposition(bytes.slice(numReadBytes + i * Constants25519.PubKeyLength,
        numReadBytes + (i + 1) * Constants25519.PubKeyLength))
    }.toMap

    numReadBytes += keyMappingSize * Constants25519.PubKeyLength

    val roleTypes = Map[Int, Role.Role](
      0 -> Role.Producer,
      1 -> Role.Investor,
      2 -> Role.Hub
    )

    val parties: Map[PublicKey25519Proposition, Role.Role] = (0 until partiesLength).map { i =>
      val pkInt = Ints.fromByteArray(bytes.slice(numReadBytes + 2 * i * Ints.BYTES,
        numReadBytes + (2 * i + 1) * Ints.BYTES))
      val roleInt = Ints.fromByteArray(bytes.slice(numReadBytes + (2 * i + 1) * Ints.BYTES,
        numReadBytes + 2 * (i + 1) * Ints.BYTES))
      keyMapping(pkInt) -> roleTypes(roleInt)
    }.toMap

    numReadBytes += partiesLength * (Ints.BYTES * 2)

    val signatures: Map[PublicKey25519Proposition, Signature25519] = (0 until sigLength).map { i =>
      val pkInt = Ints.fromByteArray(bytes.slice(numReadBytes + i * (Ints.BYTES + Curve25519.SignatureLength),
        numReadBytes + i * Curve25519.SignatureLength + (i + 1) * Ints.BYTES))
      val sigBytes = bytes.slice(numReadBytes + (i + 1) * Ints.BYTES + i * Curve25519.SignatureLength,
        numReadBytes + (i + 1) * Ints.BYTES + (i + 1) * Curve25519.SignatureLength)
      keyMapping(pkInt) -> Signature25519(sigBytes)
    }.toMap

    numReadBytes += sigLength * (Ints.BYTES + Curve25519.SignatureLength)

    val feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]] = (0 until feePreBoxLength).map { i =>
      var bytesSoFar = 0
      val pkInt = Ints.fromByteArray(bytes.slice(numReadBytes + bytesSoFar, numReadBytes + bytesSoFar + Ints.BYTES))
      bytesSoFar += Ints.BYTES

      val length = Ints.fromByteArray(bytes.slice(numReadBytes + bytesSoFar, numReadBytes + bytesSoFar + Ints.BYTES))
      bytesSoFar += Ints.BYTES

      val preBoxes = (0 until length).map { j =>
        var innerBytesSoFar = j * 2 * Longs.BYTES
        val nonce = Longs.fromByteArray(bytes.slice(numReadBytes + bytesSoFar + innerBytesSoFar,
          numReadBytes + bytesSoFar + innerBytesSoFar + Longs.BYTES))
        val amount = Longs.fromByteArray(bytes.slice(numReadBytes + bytesSoFar + innerBytesSoFar + Longs.BYTES,
          numReadBytes + bytesSoFar + innerBytesSoFar + 2 * Longs.BYTES))
        nonce -> amount
      }
      numReadBytes += bytesSoFar + length * 2 * Longs.BYTES
      keyMapping(pkInt) -> preBoxes
    }.toMap

    val fees: Map[PublicKey25519Proposition, Long] = (0 until feesLength).map { i =>
      val pkInt = Ints.fromByteArray(bytes.slice(numReadBytes + i * (Ints.BYTES + Longs.BYTES),
        numReadBytes + i * (Ints.BYTES + Longs.BYTES) + Ints.BYTES))
      val fee = Longs.fromByteArray(bytes.slice(numReadBytes + i * (Ints.BYTES + Longs.BYTES) + Ints.BYTES,
        numReadBytes + (i + 1) * Ints.BYTES + (i + 1) * Longs.BYTES))
      keyMapping(pkInt) -> fee
    }.toMap

    (parties, signatures, feePreBoxes, fees, timestamp)
  }
}
