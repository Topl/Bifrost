//accumulators co.topl.modifier.transaction.serialization
//
//import co.topl.attestation.proposition.{PublicKeyPropositionCurve25519, PublicKeyPropositionCurve25519Serializer}
//import co.topl.attestation.proof.{SignatureCurve25519, SignatureCurve25519Serializer}
//import co.topl.modifier.transaction.ProgramMethodExecution
//import co.topl.modifier.transaction.Transaction.Nonce
//import co.topl.modifier.box._
//import co.topl.modifier.box.serialization.{CodeBoxSerializer, ExecutionBoxSerializer, StateBoxSerializer}
//import co.topl.utils.Extensions._
//import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}
//import io.circe.{Json, parser}
//
//object ProgramMethodExecutionSerializer extends BifrostSerializer[ProgramMethodExecution] {
//
//  override def serialize(obj: ProgramMethodExecution, w: Writer): Unit = {
//    /* executionBox: ExecutionBox */
//    ExecutionBoxSerializer.serialize(obj.executionBox, w)
//
//    /* state: Seq[StateBox] */
//    w.putUInt(obj.stateBoxes.length)
//    obj.stateBoxes.foreach(stateBox => StateBoxSerializer.serialize(stateBox, w))
//
//    /* code: Seq[CodeBox] */
//    w.putUInt(obj.codeBoxes.length)
//    obj.codeBoxes.foreach(codeBox => CodeBoxSerializer.serialize(codeBox, w))
//
//    /* methodName: String */
//    w.putByteString(obj.methodName)
//
//    /* methodParams: Json */
//    w.putIntString(obj.methodParams.noSpaces)
//
//    /* owner: PublicKey25519Proposition */
//    PublicKeyPropositionCurve25519Serializer.serialize(obj.owner, w)
//
//    /* signatures: Map[PublicKey25519Proposition, Signature25519] */
//    SignatureCurve25519Serializer.serialize(obj.signatures.head._2, w)
//
//
//    // TODO: Jing - preFeeBoxes will be removed
//    /* preFeeBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]] */
//    /* nonce can be negative and value is positive */
//    w.putUInt(obj.preFeeBoxes.head._2.length)
//    obj.preFeeBoxes.head._2.foreach { case (nonce, value) =>
//      w.putLong(nonce)
//      w.putULong(value)
//    }
//
//    /* fees: Map[PublicKey25519Proposition, Long] */
//    w.putULong(obj.fees.head._2)
//
//    /* timestamp: Long */
//    w.putULong(obj.timestamp)
//
//    /* data: String */
//    w.putIntString(obj.data)
//  }
//
//  override def parse(r: Reader): ProgramMethodExecution = {
//    val executionBox: ExecutionBox = ExecutionBoxSerializer.parse(r)
//
//    val stateLength: Int = r.getUInt().toIntExact
//    val state: Seq[StateBox] = (0 until stateLength).map(_ => StateBoxSerializer.parse(r))
//
//    val codeLength: Int = r.getUInt().toIntExact
//    val code: Seq[CodeBox] = (0 until codeLength).map(_ => CodeBoxSerializer.parse(r))
//
//    val methodName: String = r.getByteString()
//
//    val methodParams: Json = parser.parse(r.getIntString()) match {
//      case Left(f) => throw f
//      case Right(j: Json) => j
//    }
//
//    val owner: PublicKeyPropositionCurve25519 = PublicKeyPropositionCurve25519Serializer.parse(r)
//
//    val signatures: Map[PublicKeyPropositionCurve25519, SignatureCurve25519] = {
//      val sig = SignatureCurve25519Serializer.parse(r)
//      Map(owner -> sig)
//    }
//
//    val preBoxesLength: Int = r.getUInt().toIntExact
//    val preBoxes: IndexedSeq[(Nonce, Long)] = (0 until preBoxesLength).map { _ =>
//      val nonce: Nonce = r.getLong()
//      val value: Long = r.getULong()
//      nonce -> value
//    }
//    val preFeeBoxes: Map[PublicKeyPropositionCurve25519, IndexedSeq[(Nonce, Long)]] = Map(owner -> preBoxes)
//
//    val fees: Map[PublicKeyPropositionCurve25519, Long] = Map(owner -> r.getULong())
//    val timestamp: Long = r.getULong()
//    val data: String = r.getIntString()
//
//    ProgramMethodExecution(executionBox, state, code, methodName, methodParams,
//                           owner, signatures, preFeeBoxes, fees, timestamp, data)
//  }
//}
