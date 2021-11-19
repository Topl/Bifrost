package co.topl.codecs.binary.legacy.modifier.transaction

import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.modifier.transaction._

import scala.util.{Failure, Success}

object TransactionSerializer extends BifrostSerializer[Transaction.TX] {

  override def serialize(obj: Transaction.TX, w: Writer): Unit =
    obj match {
      case obj: ArbitTransfer[_] =>
        w.put(ArbitTransfer.typePrefix)
        ArbitTransferSerializer.serialize(obj, w)

      case obj: PolyTransfer[_] =>
        w.put(PolyTransfer.typePrefix)
        PolyTransferSerializer.serialize(obj, w)

      case obj: AssetTransfer[_] =>
        w.put(AssetTransfer.typePrefix)
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

  override def parse(r: Reader): Transaction.TX =
    (r.getByte() match {
      case ArbitTransfer.typePrefix => ArbitTransferSerializer.parseTry(r)
      case PolyTransfer.typePrefix  => PolyTransferSerializer.parseTry(r)
      case AssetTransfer.typePrefix => AssetTransferSerializer.parseTry(r)
//      case CodeCreation.txTypePrefix  => CodeCreationTransferSerializer.parse(r)
//      case ProgramCreation.txTypePrefix  => ProgramCreationTransferSerializer.parse(r)
//      case ProgramMethodExecution.txTypePrefix  => ProgramMethodExecutionTransferSerializer.parse(r)
//      case ProgramTransfer.txTypePrefix  => ProgramTransferTransferSerializer.parse(r)
    }) match {
      case Success(tx) => tx
      case Failure(ex) => throw ex
    }
}
