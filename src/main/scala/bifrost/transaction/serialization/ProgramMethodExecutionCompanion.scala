package bifrost.transaction.serialization

import bifrost.serialization.Serializer
import bifrost.transaction.bifrostTransaction.BifrostTransaction.Nonce
import bifrost.transaction.bifrostTransaction
import bifrost.transaction.bifrostTransaction.ProgramMethodExecution
import bifrost.transaction.box.{ExecutionBox, ExecutionBoxSerializer}
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import com.google.common.primitives.{Bytes, Ints}
import io.circe.Json
import io.circe.parser.parse

import scala.util.Try

object ProgramMethodExecutionCompanion extends Serializer[ProgramMethodExecution] {

  override def toBytes(cme: ProgramMethodExecution): Array[Byte] = {
    ProgramTransactionCompanion.prefixBytes ++ toChildBytes(cme)
  }

  def toChildBytes(cme: ProgramMethodExecution): Array[Byte] = {
    val typeBytes = "ProgramMethodExecution".getBytes

    Bytes.concat(
      /* First two arguments MUST STAY */
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      Ints.toByteArray(cme.methodName.getBytes.length),
      Ints.toByteArray(cme.parameters.noSpaces.getBytes.length),
      cme.methodName.getBytes,
      cme.parameters.noSpaces.getBytes,
      ProgramTransactionCompanion.commonToBytes(cme),
      cme.data.getBytes,
      Ints.toByteArray(cme.data.getBytes.length)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[ProgramMethodExecution] = Try {
    val dataLen: Int = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES, bytes.length))
    val data: String = new String(
      bytes.slice(bytes.length - Ints.BYTES - dataLen, bytes.length - Ints.BYTES))
    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))

    var numReadBytes = Ints.BYTES + typeLength
    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)

    val Array(methodNameLength: Int, parameterJsonLength: Int, programBoxLength: Int) = (0 until 3).map { i =>
      Ints.fromByteArray(bytesWithoutType.slice(i * Ints.BYTES, (i + 1) * Ints.BYTES))
    }.toArray

    numReadBytes = 3 * Ints.BYTES

    val methodName = new String(bytesWithoutType.slice(numReadBytes, numReadBytes + methodNameLength))

    numReadBytes += methodNameLength

    val parameters: Json = parse(new String(bytesWithoutType.slice(numReadBytes,
      numReadBytes + parameterJsonLength))) match {
      case Left(f) => throw f
      case Right(j: Json) => j
    }

    numReadBytes += parameterJsonLength

    val execBox: ExecutionBox = ExecutionBoxSerializer.parseBytes(bytesWithoutType.slice(numReadBytes,
      numReadBytes + programBoxLength))
      .get

    numReadBytes += programBoxLength

    val (owner: PublicKey25519Proposition,
    signatures: Map[PublicKey25519Proposition, Signature25519],
    feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
    fees: Map[PublicKey25519Proposition, Long],
    timestamp: Long) = ProgramTransactionCompanion.commonParseBytes(bytesWithoutType.slice(numReadBytes,
      bytesWithoutType.length))

    //TODO Finish serialization for state and code boxes

    val stateBox = ???
    val codeBox = ???
    val executionBox = ???

    bifrostTransaction.ProgramMethodExecution(stateBox, codeBox, executionBox, methodName,
      parameters, owner, signatures, feePreBoxes, fees, timestamp, data)
  }

}
