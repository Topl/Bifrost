package bifrost.modifier.transaction.serialization

import java.util.UUID

import bifrost.crypto.Signature25519
import bifrost.crypto.serialization.Signature25519Serializer
import bifrost.modifier.box.proposition.{PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import bifrost.modifier.transaction.bifrostTransaction.ProgramCreation
import bifrost.modifier.transaction.bifrostTransaction.Transaction.Nonce
import bifrost.program.{ExecutionBuilder, ExecutionBuilderSerializer}
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import bifrost.utils.Extensions._
import com.google.common.primitives.{Bytes, Ints, Longs}

import scala.util.Try

//noinspection ScalaStyle
object ProgramCreationSerializer extends BifrostSerializer[ProgramCreation] {

  override def serialize(obj: ProgramCreation, w: Writer): Unit = {
    /* executionBuilder: ExecutionBuilder */
    ExecutionBuilderSerializer.serialize(obj.executionBuilder, w)

    /* readOnlyStateBoxes: Seq[UUID] */
    w.putUInt(obj.readOnlyStateBoxes.length)
    obj.readOnlyStateBoxes.foreach { id =>
      w.putLong(id.getMostSignificantBits)
      w.putLong(id.getLeastSignificantBits)
    }

    /* preInvestmentBoxes: IndexedSeq[(Nonce, Long)] */
    w.putUInt(obj.preInvestmentBoxes.length)
    obj.preInvestmentBoxes.foreach { box =>
      w.putLong(box._1)
      w.putULong(box._2)
    }

    /* owner: PublicKey25519Proposition */
    PublicKey25519PropositionSerializer.serialize(obj.owner, w)

    // TODO: Jing - We will need to change this to just the signature
    /* signatures: Map[PublicKey25519Proposition, Signature25519] */
    Signature25519Serializer.serialize(obj.signatures.head._2, w)

    // TODO: Jing - preFeeBoxes will be removed
    /* preFeeBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]] */
    /* nonce can be negative and value is positive */
    w.putUInt(obj.preFeeBoxes.head._2.length)
    obj.preFeeBoxes.head._2.foreach { case (nonce, value) =>
      w.putLong(nonce)
      w.putULong(value)
    }

    /* fees: Map[PublicKey25519Proposition, Long] */
    w.putULong(obj.fees.head._2)

    /* timestamp: Long */
    w.putULong(obj.timestamp)

    /* data: String */
    w.putIntString(obj.data)
  }

  override def parse(r: Reader): ProgramCreation = {
    val executionBuilder: ExecutionBuilder = ExecutionBuilderSerializer.parse(r)
    val readOnlyStateBoxesLength: Int = r.getUInt().toIntExact
    val readOnlyStateBoxes: Seq[UUID] = (0 until readOnlyStateBoxesLength).map(_ => new UUID(r.getLong(), r.getLong()))
    val preInvestmentBoxesLength: Int = r.getUInt().toIntExact

    val preInvestmentBoxes: IndexedSeq[(Nonce, Long)] = (0 until preInvestmentBoxesLength).map { _ =>
      val nonce: Nonce = r.getLong()
      val value: Long = r.getULong()
      nonce -> value
    }

    val owner: PublicKey25519Proposition = PublicKey25519PropositionSerializer.parse(r)

    val signatures: Map[PublicKey25519Proposition, Signature25519] = {
      val sig = Signature25519Serializer.parse(r)
      Map(owner -> sig)
    }

    val preBoxesLength: Int = r.getUInt.toIntExact
    val preBoxes: IndexedSeq[(Nonce, Long)] = (0 until preBoxesLength).map { _ =>
      val nonce: Nonce = r.getLong()
      val value: Long = r.getULong()
      nonce -> value
    }
    val preFeeBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]] = Map(owner -> preBoxes)

    val fees: Map[PublicKey25519Proposition, Long] = Map(owner -> r.getULong())
    val timestamp: Long = r.getULong()
    val data: String = r.getIntString()

    ProgramCreation(executionBuilder, readOnlyStateBoxes, preInvestmentBoxes,
                    owner, signatures, preFeeBoxes, fees, timestamp, data)
  }

// TODO: Jing - remove
//
//  override def toBytes(m: ProgramCreation): Array[Byte] = {
//    ProgramTransactionSerializer.prefixBytes ++ toChildBytes(m)
//  }
//
//  def toChildBytes(m: ProgramCreation): Array[Byte] = {
//
//    val typeBytes = "ProgramCreation".getBytes
//
//    val executionBuilderBytes = ExecutionBuilderSerializer.toBytes(m.executionBuilder)
//
//    Bytes.concat(
//      /* First two arguments MUST STAY */
//      Ints.toByteArray(typeBytes.length),
//      typeBytes,
//      Ints.toByteArray(m.preInvestmentBoxes.length),
//      m.preInvestmentBoxes.foldLeft(Array[Byte]())((a, b) => a ++ Longs.toByteArray(b._1) ++ Longs.toByteArray(b._2)),
//      Longs.toByteArray(executionBuilderBytes.length),
//      executionBuilderBytes,
//      Ints.toByteArray(m.readOnlyStateBoxes.length),
//      m.readOnlyStateBoxes.foldLeft(Array[Byte]()) {
//      (arr, x) => arr ++ Bytes.concat(
//        Longs.toByteArray(x.getMostSignificantBits),
//        Longs.toByteArray(x.getLeastSignificantBits)
//        )
//      },
//      Ints.toByteArray(m.data.getBytes.length),
//      m.data.getBytes,
//      ProgramTransactionSerializer.commonToBytes(m)
//
//    )
//  }
//
//  override def parseBytes(bytes: Array[Byte]): Try[ProgramCreation] = Try {
//
//    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
//    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))
//
//    require(typeStr == "ProgramCreation")
//
//    var numReadBytes = Ints.BYTES + typeLength
//    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)
//
//    val numPreInvestmentBoxes: Int = Ints.fromByteArray(bytesWithoutType.slice(0, Ints.BYTES))
//
//    numReadBytes = Ints.BYTES
//
//    val preInvestmentBoxes: IndexedSeq[(Nonce, Long)] = (0 until numPreInvestmentBoxes).map { i =>
//      val nonce = Longs.fromByteArray(bytesWithoutType.slice(numReadBytes + 2 * i * Longs.BYTES,
//        numReadBytes + (2 * i + 1) * Longs.BYTES))
//      val value = Longs.fromByteArray(bytesWithoutType.slice(numReadBytes + (2 * i + 1) * Longs.BYTES,
//        numReadBytes + 2 * (i + 1) * Longs.BYTES))
//      nonce -> value
//    }
//
//    numReadBytes += 2 * numPreInvestmentBoxes * Longs.BYTES
//
//    val executionBuilderLength: Long = Longs.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Longs.BYTES))
//
//    numReadBytes += Longs.BYTES
//
//    val executionBuilder = ExecutionBuilderSerializer.parseBytes(bytesWithoutType.slice(numReadBytes,
//      numReadBytes + executionBuilderLength.toInt)).get
//
//    numReadBytes += executionBuilderLength.toInt
//
//    val readOnlyStateBoxesLength = Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES))
//
//    numReadBytes += Ints.BYTES
//
//    var readOnlyStateBoxes = Seq[UUID]()
//    for (_ <- 1 to readOnlyStateBoxesLength) {
//      val uuid = new UUID(Longs.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Longs.BYTES)),
//        Longs.fromByteArray(bytesWithoutType.slice(numReadBytes + Longs.BYTES, numReadBytes + 2 * Longs.BYTES)))
//      numReadBytes += 2 * Longs.BYTES
//      readOnlyStateBoxes = readOnlyStateBoxes :+ uuid
//    }
//
//    val dataLen: Int = Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES))
//
//    numReadBytes += Ints.BYTES
//
//    val data: String = new String(
//      bytesWithoutType.slice(numReadBytes, numReadBytes + dataLen))
//
//    numReadBytes += dataLen
//
//    val (owner: PublicKey25519Proposition,
//    signatures: Map[PublicKey25519Proposition, Signature25519],
//    feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
//    fees: Map[PublicKey25519Proposition, Long],
//    timestamp: Long) = ProgramTransactionSerializer.commonParseBytes(bytesWithoutType.slice(numReadBytes,
//      bytesWithoutType.length))
//
//    ProgramCreation(executionBuilder, readOnlyStateBoxes, preInvestmentBoxes, owner, signatures, feePreBoxes, fees, timestamp, data)
//  }
}
