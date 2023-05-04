package co.topl.genusLibrary.orientDb.instances

import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.genus.services.{Txo, TxoState}
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
  }

  def make(): VertexSchema[Txo] =
    VertexSchema.create(
      Field.SchemaName,
      GraphDataEncoder[Txo]
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
          readOnly = true,
          notNull = true
        )
        .withProperty(
          Field.OutputAddress,
          _.outputAddress.toByteArray,
          mandatory = true,
          readOnly = true,
          notNull = true
        ),
      v =>
        Txo(
          transactionOutput = UnspentTransactionOutput.parseFrom(v(Field.TransactionOutput): Array[Byte]),
          state = TxoState.fromValue(v(Field.State): Int),
          outputAddress = TransactionOutputAddress.parseFrom(v(Field.OutputAddress): Array[Byte])
        )
    )

}
