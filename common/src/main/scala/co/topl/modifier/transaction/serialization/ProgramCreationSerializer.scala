//accumulators co.topl.modifier.transaction.serialization
//
//import co.topl.attestation.proposition.{PublicKeyPropositionCurve25519, PublicKeyPropositionCurve25519Serializer}
//import co.topl.attestation.proof.{SignatureCurve25519, SignatureCurve25519Serializer}
//import co.topl.modifier.transaction.ProgramCreation
//import co.topl.modifier.transaction.Transaction.Nonce
//import co.topl.modifier.box.ProgramId
//import co.topl.program.{ExecutionBuilder, ExecutionBuilderSerializer}
//import co.topl.utils.Extensions._
//import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}
//
////noinspection ScalaStyle
//object ProgramCreationSerializer extends BifrostSerializer[ProgramCreation] {
//
//  override def serialize(obj: ProgramCreation, w: Writer): Unit = {
//    /* executionBuilder: ExecutionBuilder */
//    ExecutionBuilderSerializer.serialize(obj.executionBuilder, w)
//
//    /* readOnlyStateBoxes: Seq[ProgramId] */
//    w.putUInt(obj.readOnlyStateBoxes.length)
//    obj.readOnlyStateBoxes.foreach { id =>
//      ProgramId.serialize(id, w)
//    }
//
//    /* preInvestmentBoxes: IndexedSeq[(Nonce, Long)] */
//    w.putUInt(obj.preInvestmentBoxes.length)
//    obj.preInvestmentBoxes.foreach { box =>
//      w.putLong(box._1)
//      w.putULong(box._2)
//    }
//
//    /* owner: PublicKey25519Proposition */
//    PublicKeyPropositionCurve25519Serializer.serialize(obj.owner, w)
//
//    // TODO: Jing - We will need to change this to just the signature
//    /* signatures: Map[PublicKey25519Proposition, Signature25519] */
//    SignatureCurve25519Serializer.serialize(obj.signatures.head._2, w)
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
//  override def parse(r: Reader): ProgramCreation = {
//    val executionBuilder: ExecutionBuilder = ExecutionBuilderSerializer.parse(r)
//
//    val readOnlyStateBoxesLength: Int = r.getUInt().toIntExact
//    val readOnlyStateBoxes: Seq[ProgramId] = (0 until readOnlyStateBoxesLength).map(_ => ProgramId.parse(r))
//
//    val preInvestmentBoxesLength: Int = r.getUInt().toIntExact
//    val preInvestmentBoxes: IndexedSeq[(Nonce, Long)] = (0 until preInvestmentBoxesLength).map { _ =>
//      val nonce: Nonce = r.getLong()
//      val value: Long = r.getULong()
//      nonce -> value
//    }
//
//    val owner: PublicKeyPropositionCurve25519 = PublicKeyPropositionCurve25519Serializer.parse(r)
//
//    val signatures: Map[PublicKeyPropositionCurve25519, SignatureCurve25519] = {
//      val sig = SignatureCurve25519Serializer.parse(r)
//      Map(owner -> sig)
//    }
//
//    val preBoxesLength: Int = r.getUInt.toIntExact
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
//    ProgramCreation(executionBuilder, readOnlyStateBoxes, preInvestmentBoxes,
//                    owner, signatures, preFeeBoxes, fees, timestamp, data)
//  }
//}
