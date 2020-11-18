package co.topl.modifier.transaction.serialization

import co.topl.attestation.proof.Proof
import co.topl.attestation.proposition.{KnowledgeProposition, Proposition}
import co.topl.attestation.secrets.Secret
import co.topl.modifier.transaction._
import co.topl.nodeView.state.box.{Box, GenericBox}
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

import scala.util.{Failure, Success}

object TransactionSerializer extends BifrostSerializer[Transaction[_, _ <: Proposition]] {

  override def serialize(obj: Transaction[_, _ <: Proposition], w: Writer): Unit = {
    obj match {
      case obj: ArbitTransfer[_] =>
        w.put(ArbitTransfer.txTypePrefix)
        ArbitTransferSerializer.serialize(obj, w)

      case obj: PolyTransfer[_] =>
        w.put(PolyTransfer.txTypePrefix)
        PolyTransferSerializer.serialize(obj, w)

      case obj: AssetTransfer[_] =>
        w.put(AssetTransfer.txTypePrefix)
        AssetTransferSerializer.serialize(obj, w)

//      case obj: CodeCreation =>
//        w.put(CodeCreation.txTypePrefix)
//        CodeBoxCreationSerializer.serialize(obj, w)
//
//      case obj: ProgramCreation =>
//        w.put(ProgramCreation.txTypePrefix)
//        ProgramCreationSerializer.serialize(obj, w)
//
//      case obj: ProgramMethodExecution =>
//        w.put(PolyTrProgramMethodExecutionansfer.txTypePrefix)
//        ProgramMethodExecutionSerializer.serialize(obj, w)
//
//      case obj: ProgramTransfer =>
//        w.put(ProgramTransfer.txTypePrefix)
//        ProgramTransferSerializer.serialize(obj, w)
    }
  }

  override def parse(r: Reader): Transaction[_, _ <: Proposition] = {
    (r.getByte() match {
      case ArbitTransfer.txTypePrefix => ArbitTransferSerializer.parseTry(r)
      case PolyTransfer.txTypePrefix  => PolyTransferSerializer.parseTry(r)
      case AssetTransfer.txTypePrefix  => AssetTransferSerializer.parseTry(r)
//      case CodeCreation.txTypePrefix  => CodeCreationTransferSerializer.parse(r)
//      case ProgramCreation.txTypePrefix  => ProgramCreationTransferSerializer.parse(r)
//      case ProgramMethodExecution.txTypePrefix  => ProgramMethodExecutionTransferSerializer.parse(r)
//      case ProgramTransfer.txTypePrefix  => ProgramTransferTransferSerializer.parse(r)
    }) match {
      case Success(tx) => tx
      case Failure(ex) => throw ex
    }
  }
}
