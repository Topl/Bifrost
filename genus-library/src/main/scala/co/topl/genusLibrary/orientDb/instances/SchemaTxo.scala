package co.topl.genusLibrary.orientDb.instances

import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.genus.services.{Txo, TxoState}
import co.topl.genusLibrary.orientDb.schema.OIndexable.Instances.txo
import co.topl.genusLibrary.orientDb.schema.OTyped.Instances._
import co.topl.genusLibrary.orientDb.schema.{GraphDataEncoder, VertexSchema}

object SchemaTxo {

  /**
   * Txo model:
   * @see https://github.com/Topl/protobuf-specs/blob/main/proto/genus/genus_models.proto#L28
   */
  object Field {
    val SchemaName = "Txo"
    val TransactionOutput = "transactionOutput"
    val State = "state"
    val OutputAddress = "outputAddress"
    val TxoId = "txoId"
    val TxoIndex = "txoIndex"
  }

  def make(): VertexSchema[Txo] =
    VertexSchema.create(
      Field.SchemaName,
      GraphDataEncoder[Txo]
        .withProperty(
          Field.TxoId,
          txo => txo.outputAddress.id.value.toByteArray :+ txo.outputAddress.index.byteValue,
          mandatory = true,
          readOnly = true,
          notNull = true
        )
        .withProperty(
          Field.TransactionOutput,
          _.transactionOutput.toByteArray,
          mandatory = true,
          readOnly = true,
          notNull = true
        )
        .withProperty(
          Field.State,
          txo => java.lang.Integer.valueOf(txo.state.value),
          mandatory = true,
          readOnly = false,
          notNull = true
        )
        .withProperty(
          Field.OutputAddress,
          _.outputAddress.toByteArray,
          mandatory = true,
          readOnly = true,
          notNull = true
        )
        .withIndex[Txo](Field.TxoIndex, Field.TxoId),
      v =>
        Txo(
          transactionOutput = UnspentTransactionOutput.parseFrom(v(Field.TransactionOutput): Array[Byte]),
          state = TxoState.fromValue(v(Field.State): Int),
          outputAddress = TransactionOutputAddress.parseFrom(v(Field.OutputAddress): Array[Byte])
        )
    )

}
