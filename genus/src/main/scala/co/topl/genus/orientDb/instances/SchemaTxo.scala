package co.topl.genus.orientDb.instances

import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.brambl.models.{TransactionInputAddress, TransactionOutputAddress}
import co.topl.brambl.syntax._
import co.topl.genus.orientDb.schema.{GraphDataEncoder, OIndexable, VertexSchema}
import co.topl.genus.services.{Txo, TxoState}
import VertexSchemaInstances.instances._
import co.topl.genus.orientDb.schema.OTyped.Instances._
import com.orientechnologies.orient.core.metadata.schema.OType
import com.tinkerpop.blueprints.Vertex

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
    val SpendingTransaction = "spendingTransaction"
    // This is the _property_
    val SpendingInputIndex = "spendingInputIndex"
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
        .withIndex[Txo](Field.TxoIndex, Field.TxoId)(OIndexable.Instances.txo)
        .withLink(Field.SpendingTransaction, OType.LINK, SchemaIoTransaction.Field.SchemaName)
        .withProperty(
          Field.SpendingInputIndex,
          txo => txo.spender.map(s => java.lang.Integer.valueOf(s.inputAddress.index)).orNull,
          mandatory = false,
          readOnly = false,
          notNull = false
        ),
      v =>
        Txo(
          transactionOutput = UnspentTransactionOutput.parseFrom(v(Field.TransactionOutput): Array[Byte]),
          state = TxoState.fromValue(v(Field.State): Int),
          outputAddress = TransactionOutputAddress.parseFrom(v(Field.OutputAddress): Array[Byte]),
          spender = Option(v.vertex.getProperty[java.lang.Integer](SchemaTxo.Field.SpendingInputIndex))
            .flatMap(index =>
              Option(v.vertex.getProperty[Vertex](SchemaTxo.Field.SpendingTransaction))
                .map(ioTransactionSchema.decode)
                .map { tx =>
                  val input = tx.inputs(index)
                  val inputAddress =
                    TransactionInputAddress(input.address.network, input.address.ledger, index, tx.id)
                  Txo.Spender(inputAddress, input)
                }
            )
        )
    )

}
