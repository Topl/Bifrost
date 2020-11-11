package co.topl.modifier.transaction.serialization

import co.topl.attestation.proof.Proof
import co.topl.attestation.proposition.{ KnowledgeProposition, Proposition }
import co.topl.attestation.secrets.Secret
import co.topl.modifier.transaction._
import co.topl.nodeView.state.box.{ Box, GenericBox }
import co.topl.utils.serialization.{ BifrostSerializer, Reader, Writer }

object TransactionSerializer extends BifrostSerializer[Transaction[_, _ <: Proposition, _ <: Proof[_], _ <: Box[_]]] {

  override def serialize( obj: Transaction[_, _ <: Proposition, _ <: Proof[_], _ <: Box[_]], w: Writer): Unit = {
    obj match {
      case obj: ArbitTransfer[_ <: Proposition, _ <: Proof[_]] =>
        w.put(ArbitTransfer.txTypePrefix)
        ArbitTransferSerializer.serialize(obj, w)

      case obj: PolyTransfer[_ <: Proposition, _ <: Proof[_]] =>
        w.put(PolyTransfer.txTypePrefix)
        PolyTransferSerializer.serialize(obj, w)

      case obj: AssetTransfer[_ <: Proposition, _ <: Proof[_]] =>
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

  override def parse(r: Reader): Transaction[_, _ <: Proposition, _ <: Proof[_], _ <: Box[_]] = {
    r.getByte() match {
      case ArbitTransfer.txTypePrefix => ArbitTransferSerializer.parse(r)
      case PolyTransfer.txTypePrefix  => PolyTransferSerializer.parse(r)
      case AssetTransfer.txTypePrefix  => AssetTransferSerializer.parse(r)
//      case CodeCreation.txTypePrefix  => CodeCreationTransferSerializer.parse(r)
//      case ProgramCreation.txTypePrefix  => ProgramCreationTransferSerializer.parse(r)
//      case ProgramMethodExecution.txTypePrefix  => ProgramMethodExecutionTransferSerializer.parse(r)
//      case ProgramTransfer.txTypePrefix  => ProgramTransferTransferSerializer.parse(r)
    }
  }
}
