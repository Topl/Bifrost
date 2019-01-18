package bifrost.transaction.serialization

import bifrost.serialization.Serializer
import bifrost.transaction.BifrostTransaction.Nonce
import bifrost.transaction.Role.Role
import bifrost.transaction.bifrostTransaction.ContractCreation
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import bifrost.transaction.{AgreementCompanion, ContractTransactionCompanion}
import com.google.common.primitives.{Bytes, Ints, Longs}

import scala.util.Try

//noinspection ScalaStyle
object ContractCreationCompanion extends Serializer[ContractCreation] {

  override def toBytes(m: ContractCreation): Array[Byte] = {
    ContractTransactionCompanion.prefixBytes ++ toChildBytes(m)
  }

  def toChildBytes(m: ContractCreation): Array[Byte] = {
    val typeBytes = "ContractCreation".getBytes

    val agreementBytes = AgreementCompanion.toBytes(m.agreement)

    Bytes.concat(
      /* First two arguments MUST STAY */
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      Ints.toByteArray(m.preInvestmentBoxes.length),
      m.preInvestmentBoxes.foldLeft(Array[Byte]())((a, b) => a ++ Longs.toByteArray(b._1) ++ Longs.toByteArray(b._2)),
      Longs.toByteArray(agreementBytes.length),
      agreementBytes,
      ContractTransactionCompanion.commonToBytes(m),
      m.data.getBytes,
      Ints.toByteArray(m.data.getBytes.length)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[ContractCreation] = Try {
    val dataLen: Int = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES, bytes.length))
    val data: String = new String(
      bytes.slice(bytes.length - Ints.BYTES - dataLen, bytes.length - Ints.BYTES))
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

    val agreementLength: Long = Longs.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Longs.BYTES))

    numReadBytes += Longs.BYTES

    val agreement = AgreementCompanion.parseBytes(bytesWithoutType.slice(numReadBytes,
      numReadBytes + agreementLength.toInt)).get

    numReadBytes += agreementLength.toInt

    val (parties: Map[PublicKey25519Proposition, Role],
    signatures: Map[PublicKey25519Proposition, Signature25519],
    feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
    fees: Map[PublicKey25519Proposition, Long],
    timestamp: Long) = ContractTransactionCompanion.commonParseBytes(bytesWithoutType.slice(numReadBytes,
      bytesWithoutType.length))

    ContractCreation(agreement, preInvestmentBoxes, parties, signatures, feePreBoxes, fees, timestamp, data)
  }

}
