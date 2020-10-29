package bifrost.modifier.transaction.serialization

import bifrost.crypto.Signature25519
import bifrost.crypto.serialization.Signature25519Serializer
import bifrost.modifier.box._
import bifrost.modifier.box.proposition.{PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import bifrost.modifier.box.serialization.{CodeBoxSerializer, ExecutionBoxSerializer, StateBoxSerializer}
import bifrost.modifier.transaction.bifrostTransaction.ProgramMethodExecution
import bifrost.modifier.transaction.bifrostTransaction.Transaction.Nonce
import bifrost.utils.Extensions._
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import io.circe.{parser, Json}

object ProgramMethodExecutionSerializer extends BifrostSerializer[ProgramMethodExecution] {

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
      case Left(f)        => throw f
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

    ProgramMethodExecution(
      state,
      code,
      executionBox,
      methodName,
      methodParams,
      owner,
      signatures,
      preFeeBoxes,
      fees,
      timestamp,
      data
    )
  }
}
