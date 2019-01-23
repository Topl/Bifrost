package bifrost.transaction.serialization

import bifrost.serialization.Serializer
import bifrost.transaction.BifrostTransaction.Nonce
import bifrost.transaction.Role.Role
import bifrost.transaction.bifrostTransaction
import bifrost.transaction.bifrostTransaction.ContractCompletion
import bifrost.transaction.box.proposition.{Constants25519, PublicKey25519Proposition}
import bifrost.transaction.box.{ContractBox, ContractBoxSerializer, ReputationBox}
import bifrost.transaction.proof.Signature25519
import com.google.common.primitives.{Bytes, Doubles, Ints, Longs}

import scala.util.Try

object ContractCompletionCompanion extends Serializer[ContractCompletion] {

  override def toBytes(cc: ContractCompletion): Array[Byte] = {
    ContractTransactionCompanion.prefixBytes ++ toChildBytes(cc)
  }

  def toChildBytes(cc: ContractCompletion): Array[Byte] = {
    val typeBytes = "ContractCompletion".getBytes

    Bytes.concat(
      /* First two arguments MUST STAY */
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      Ints.toByteArray(cc.producerReputation.length),
      Ints.toByteArray(cc.contractBox.bytes.length),
      cc.producerReputation.foldLeft(Array[Byte]())((a, b) =>
        a ++ b.proposition.pubKeyBytes ++ Longs.toByteArray(b.nonce) ++ doubleToByteArray(b.value._1) ++ doubleToByteArray(b.value._2)
      ),
      cc.contractBox.bytes,
      ContractTransactionCompanion.commonToBytes(cc),
      cc.data.getBytes,
      Ints.toByteArray(cc.data.getBytes.length)
    )
  }

  //noinspection ScalaStyle
  override def parseBytes(bytes: Array[Byte]): Try[ContractCompletion] = Try {
    val dataLen: Int = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES, bytes.length))
    val data: String = new String(
      bytes.slice(bytes.length - Ints.BYTES - dataLen, bytes.length - Ints.BYTES))
    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))
    var numReadBytes = Ints.BYTES + typeLength
    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)


    val Array(reputationLength: Int, contractBoxLength: Int) = (0 until 2).map { i =>
      Ints.fromByteArray(bytesWithoutType.slice(i * Ints.BYTES, (i + 1) * Ints.BYTES))
    }.toArray

    numReadBytes = 2 * Ints.BYTES

    val producerReputation: IndexedSeq[ReputationBox] = (0 until reputationLength) map { i =>
      val proposition = PublicKey25519Proposition(bytesWithoutType.slice(
        numReadBytes + i*(Constants25519.PubKeyLength + Longs.BYTES + 2*Doubles.BYTES),
        numReadBytes + i*(Constants25519.PubKeyLength + Longs.BYTES + 2*Doubles.BYTES) + Constants25519.PubKeyLength
      ))

      val nonce = Longs.fromByteArray(bytesWithoutType.slice(
        numReadBytes + i*(Constants25519.PubKeyLength + Longs.BYTES + 2*Doubles.BYTES) + Constants25519.PubKeyLength,
        numReadBytes + i*(Constants25519.PubKeyLength + Longs.BYTES + 2*Doubles.BYTES) + Constants25519.PubKeyLength + Longs.BYTES
      ))

      val Array(alpha: Double, beta: Double) = (0 until 2).map { j =>
        byteArrayToDouble(
          bytesWithoutType.slice(
            numReadBytes + (i + 1)*(Constants25519.PubKeyLength + Longs.BYTES) + 2*i*Doubles.BYTES + j*Doubles.BYTES,
            numReadBytes + (i + 1)*(Constants25519.PubKeyLength + Longs.BYTES) + 2*i*Doubles.BYTES + (j + 1)*Doubles.BYTES
          )
        )
      }.toArray

      ReputationBox(proposition, nonce, (alpha, beta))
    }

    numReadBytes += reputationLength * (Constants25519.PubKeyLength + Longs.BYTES + 2 * Doubles.BYTES)

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

    bifrostTransaction.ContractCompletion(contractBox, producerReputation, parties, signatures, feePreBoxes, fees, timestamp, data)
  }

  def doubleToByteArray(x: Double): Array[Byte] = {
    val l = java.lang.Double.doubleToLongBits(x)
    val a = Array.fill(8)(0.toByte)
    for (i <- 0 to 7) a(i) = ((l >> ((7 - i) * 8)) & 0xff).toByte
    a
  }

  def byteArrayToDouble(x: Array[scala.Byte]): Double = {
    var res = 0.toLong
    for (i <- 0 to 7) {
      res += ((x(i) & 0xff).toLong << ((7 - i) * 8))
    }
    java.lang.Double.longBitsToDouble(res)
  }

}
