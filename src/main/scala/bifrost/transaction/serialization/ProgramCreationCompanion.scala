package bifrost.transaction.serialization

import java.util.UUID

import bifrost.serialization.Serializer
import bifrost.transaction.bifrostTransaction.BifrostTransaction.Nonce
import bifrost.transaction.bifrostTransaction.ProgramCreation
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import com.google.common.primitives.{Bytes, Ints, Longs}

import scala.util.Try

//noinspection ScalaStyle
object ProgramCreationCompanion extends Serializer[ProgramCreation] {

  override def toBytes(m: ProgramCreation): Array[Byte] = {
    ProgramTransactionCompanion.prefixBytes ++ toChildBytes(m)
  }

  def toChildBytes(m: ProgramCreation): Array[Byte] = {

    val typeBytes = "ProgramCreation".getBytes

    val executionBuilderBytes = ExecutionBuilderCompanion.toBytes(m.executionBuilder)

    Bytes.concat(
      /* First two arguments MUST STAY */
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      Ints.toByteArray(m.preInvestmentBoxes.length),
      m.preInvestmentBoxes.foldLeft(Array[Byte]())((a, b) => a ++ Longs.toByteArray(b._1) ++ Longs.toByteArray(b._2)),
      Longs.toByteArray(executionBuilderBytes.length),
      executionBuilderBytes,
      Ints.toByteArray(m.readOnlyStateBoxes.length),
      m.readOnlyStateBoxes.foldLeft(Array[Byte]()) {
      (arr, x) => arr ++ Bytes.concat(
        Longs.toByteArray(x.getMostSignificantBits),
        Longs.toByteArray(x.getLeastSignificantBits)
        )
      },
      Ints.toByteArray(m.data.getBytes.length),
      m.data.getBytes,
      ProgramTransactionCompanion.commonToBytes(m)

    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[ProgramCreation] = Try {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))
    var numReadBytes = Ints.BYTES + typeLength
    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)

    val numPreInvestmentBoxes: Int = Ints.fromByteArray(bytesWithoutType.slice(0, Ints.BYTES))

    numReadBytes = Ints.BYTES

    val preInvestmentBoxes: IndexedSeq[(Nonce, Long)] = (0 until numPreInvestmentBoxes).map { i =>
      val nonce = Longs.fromByteArray(bytesWithoutType.slice(numReadBytes + 2 * i * Longs.BYTES,
        numReadBytes + (2 * i + 1) * Longs.BYTES))
      val value = Longs.fromByteArray(bytesWithoutType.slice(numReadBytes + (2 * i + 1) * Longs.BYTES,
        numReadBytes + 2 * (i + 1) * Longs.BYTES))
      nonce -> value
    }

    numReadBytes += 2 * numPreInvestmentBoxes * Longs.BYTES

    val executionBuilderLength: Long = Longs.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Longs.BYTES))

    numReadBytes += Longs.BYTES

    val executionBuilder = ExecutionBuilderCompanion.parseBytes(bytesWithoutType.slice(numReadBytes,
      numReadBytes + executionBuilderLength.toInt)).get

    numReadBytes += executionBuilderLength.toInt

    val readOnlyStateBoxesLength = Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES))

    numReadBytes += Ints.BYTES

    var readOnlyStateBoxes = Seq[UUID]()
    for (_ <- 1 to readOnlyStateBoxesLength) {
      val uuid = new UUID(Longs.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Longs.BYTES)),
        Longs.fromByteArray(bytesWithoutType.slice(numReadBytes + Longs.BYTES, numReadBytes + 2 * Longs.BYTES)))
      numReadBytes += 2 * Longs.BYTES
      readOnlyStateBoxes = readOnlyStateBoxes :+ uuid
    }

    val dataLen: Int = Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES))

    numReadBytes += Ints.BYTES

    val data: String = new String(
      bytesWithoutType.slice(numReadBytes, numReadBytes + dataLen))

    numReadBytes += dataLen

    val (owner: PublicKey25519Proposition,
    signatures: Map[PublicKey25519Proposition, Signature25519],
    feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
    fees: Map[PublicKey25519Proposition, Long],
    timestamp: Long) = ProgramTransactionCompanion.commonParseBytes(bytesWithoutType.slice(numReadBytes,
      bytesWithoutType.length))

    ProgramCreation(executionBuilder, readOnlyStateBoxes, preInvestmentBoxes, owner, signatures, feePreBoxes, fees, timestamp, data)
  }

}
