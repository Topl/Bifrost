package bifrost.modifier.transaction.serialization

import bifrost.crypto.{Signature25519, Signature25519Serializer}
import bifrost.modifier.box._
import bifrost.modifier.box.proposition.{PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import bifrost.modifier.transaction.bifrostTransaction
import bifrost.modifier.transaction.bifrostTransaction.ProgramMethodExecution
import bifrost.modifier.transaction.bifrostTransaction.Transaction.Nonce
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import bifrost.utils.Extensions._
import com.google.common.primitives.{Bytes, Ints}
import io.circe.{Json, parser}

import scala.util.Try

object ProgramMethodExecutionCompanion extends BifrostSerializer[ProgramMethodExecution] {

  override def serialize(obj: ProgramMethodExecution, w: Writer): Unit = {
    /* state: Seq[StateBox] */
    w.putUInt(obj.state.length)
    obj.state.foreach(stateBox => StateBoxSerializer.serialize(stateBox, w))

    /* code: Seq[CodeBox] */
    w.putUInt(obj.code.length)
    obj.code.foreach(codeBox => CodeBoxSerializer.serialize(codeBox, w))

    /* executionBox: ExecutionBox */
    ExecutionBoxSerializer.serialize(obj.executionBox, w)

    /* methodName: String */
    w.putByteString(obj.methodName)

    /* methodParams: Json */
    w.putIntString(obj.methodParams.noSpaces)

    /* owner: PublicKey25519Proposition */
    PublicKey25519PropositionSerializer.serialize(obj.owner, w)

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

  override def parse(r: Reader): ProgramMethodExecution = {
    val stateLength: Int = r.getUInt().toIntExact
    val state: Seq[StateBox] = (0 until stateLength).map(_ => StateBoxSerializer.parse(r))
    val codeLength: Int = r.getUInt().toIntExact
    val code: Seq[CodeBox] = (0 until codeLength).map(_ => CodeBoxSerializer.parse(r))
    val executionBox: ExecutionBox = ExecutionBoxSerializer.parse(r)
    val methodName: String = r.getByteString()

    val methodParams: Json = parser.parse(r.getIntString()) match {
      case Left(f) => throw f
      case Right(j: Json) => j
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

    ProgramMethodExecution(state, code, executionBox, methodName, methodParams,
                           owner, signatures, preFeeBoxes, fees, timestamp, data)
  }

// TODO: Jing - remove
//
//  override def toBytes(cme: ProgramMethodExecution): Array[Byte] = {
//    ProgramTransactionCompanion.prefixBytes ++ toChildBytes(cme)
//  }
//
//  def toChildBytes(cme: ProgramMethodExecution): Array[Byte] = {
//    val typeBytes = "ProgramMethodExecution".getBytes
//
//    Bytes.concat(
//      /* First two arguments MUST STAY */
//      Ints.toByteArray(typeBytes.length),
//      typeBytes,
//      Ints.toByteArray(cme.state.length),
//      Ints.toByteArray(cme.code.length),
//      Ints.toByteArray(cme.executionBox.bytes.length),
//      Ints.toByteArray(cme.methodName.getBytes.length),
//      Ints.toByteArray(cme.methodParams.noSpaces.getBytes.length),
//      cme.state.foldLeft(Array[Byte]())((a,b) => a ++ Ints.toByteArray(b.bytes.length) ++ b.bytes),
//      cme.code.foldLeft(Array[Byte]())((a,b) => a ++ Ints.toByteArray(b.bytes.length) ++ b.bytes),
//      cme.executionBox.bytes,
//      cme.methodName.getBytes,
//      cme.methodParams.noSpaces.getBytes,
//      Ints.toByteArray(cme.data.getBytes.length),
//      cme.data.getBytes,
//      ProgramTransactionCompanion.commonToBytes(cme)
//    )
//  }
//
//  override def parseBytes(bytes: Array[Byte]): Try[ProgramMethodExecution] = Try {
//    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
//    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))
//
//    require(typeStr == "ProgramMethodExecution")
//
//    var numReadBytes = Ints.BYTES + typeLength
//
//    val Array(stateLength: Int, codeLength: Int, executionBoxLength: Int, methodNameLength: Int, parametersLength: Int) =
//      (0 until 5).map { i =>
//        Ints.fromByteArray(bytes.slice(numReadBytes + i * Ints.BYTES, numReadBytes + (i + 1) * Ints.BYTES))
//      }.toArray
//
//    numReadBytes += 5 * Ints.BYTES
//
//    val state: Seq[StateBox] = (0 until stateLength).map { _ =>
//      val stateBoxLength = Ints.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Ints.BYTES))
//      numReadBytes += Ints.BYTES
//      val stateBox = StateBoxSerializer.parseBytes(bytes.slice(numReadBytes, numReadBytes + stateBoxLength)).get
//      numReadBytes += stateBoxLength
//      stateBox
//    }
//
//    val code: Seq[CodeBox] = (0 until codeLength).map { _ =>
//      val codeBoxLength = Ints.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Ints.BYTES))
//      numReadBytes += Ints.BYTES
//      val codeBox = CodeBoxSerializer.parseBytes(bytes.slice(numReadBytes, numReadBytes + codeBoxLength)).get
//      numReadBytes += codeBoxLength
//      codeBox
//    }
//
//    val executionBox = ExecutionBoxSerializer.parseBytes(bytes.slice(numReadBytes,
//      numReadBytes + executionBoxLength))
//      .get
//
//    numReadBytes += executionBoxLength
//
//    val methodName = new String(bytes.slice(numReadBytes, numReadBytes + methodNameLength))
//
//    numReadBytes += methodNameLength
//
//    val methodParams: Json = parser.parse(new String(bytes.slice(numReadBytes,
//      numReadBytes + parametersLength))) match {
//      case Left(f) => throw f
//      case Right(j: Json) => j
//    }
//
//    numReadBytes += parametersLength
//
//    val dataLen: Int = Ints.fromByteArray(bytes.slice(numReadBytes, numReadBytes + Ints.BYTES))
//
//    numReadBytes += Ints.BYTES
//
//    val data: String = new String(bytes.slice(numReadBytes, numReadBytes + dataLen))
//
//    numReadBytes += dataLen
//
//    val (owner: PublicKey25519Proposition,
//    signatures: Map[PublicKey25519Proposition, Signature25519],
//    feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
//    fees: Map[PublicKey25519Proposition, Long],
//    timestamp: Long) = ProgramTransactionCompanion.commonParseBytes(bytes.slice(numReadBytes,
//      bytes.length))
//
//    bifrostTransaction.ProgramMethodExecution(state, code, executionBox, methodName,
//      methodParams, owner, signatures, feePreBoxes, fees, timestamp, data)
//  }
}
