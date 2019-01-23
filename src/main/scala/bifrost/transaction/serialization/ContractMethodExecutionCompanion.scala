package bifrost.transaction.serialization

import bifrost.serialization.Serializer
import bifrost.transaction.BifrostTransaction.Nonce
import bifrost.transaction.Role.Role
import bifrost.transaction.bifrostTransaction
import bifrost.transaction.bifrostTransaction.ContractMethodExecution
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.box.{ContractBox, ContractBoxSerializer}
import bifrost.transaction.proof.Signature25519
import com.google.common.primitives.{Bytes, Ints}
import io.circe.Json
import io.circe.parser.parse

import scala.util.Try

object ContractMethodExecutionCompanion extends Serializer[ContractMethodExecution] {

  override def toBytes(cme: ContractMethodExecution): Array[Byte] = {
    ContractTransactionCompanion.prefixBytes ++ toChildBytes(cme)
  }

  def toChildBytes(cme: ContractMethodExecution): Array[Byte] = {
    val typeBytes = "ContractMethodExecution".getBytes

    Bytes.concat(
      /* First two arguments MUST STAY */
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      Ints.toByteArray(cme.methodName.getBytes.length),
      Ints.toByteArray(cme.parameters.noSpaces.getBytes.length),
      Ints.toByteArray(cme.contractBox.bytes.length),
      cme.methodName.getBytes,
      cme.parameters.noSpaces.getBytes,
      cme.contractBox.bytes,
      ContractTransactionCompanion.commonToBytes(cme),
      cme.data.getBytes,
      Ints.toByteArray(cme.data.getBytes.length)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[ContractMethodExecution] = Try {
    val dataLen: Int = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES, bytes.length))
    val data: String = new String(
      bytes.slice(bytes.length - Ints.BYTES - dataLen, bytes.length - Ints.BYTES))
    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))

    var numReadBytes = Ints.BYTES + typeLength
    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)

    val Array(methodNameLength: Int, parameterJsonLength: Int, contractBoxLength: Int) = (0 until 3).map { i =>
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

    val contractBox: ContractBox = ContractBoxSerializer.parseBytes(bytesWithoutType.slice(numReadBytes,
      numReadBytes + contractBoxLength))
      .get

    numReadBytes += contractBoxLength

    val (parties: Map[PublicKey25519Proposition, Role],
    signatures: Map[PublicKey25519Proposition, Signature25519],
    feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
    fees: Map[PublicKey25519Proposition, Long],
    timestamp: Long) = ContractTransactionCompanion.commonParseBytes(bytesWithoutType.slice(numReadBytes,
      bytesWithoutType.length))

    bifrostTransaction.ContractMethodExecution(contractBox, methodName, parameters, parties, signatures, feePreBoxes, fees, timestamp, data)
  }

}
