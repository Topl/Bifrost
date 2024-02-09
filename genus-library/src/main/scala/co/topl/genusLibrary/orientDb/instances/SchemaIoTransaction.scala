package co.topl.genusLibrary.orientDb.instances

import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax._
import co.topl.genusLibrary.orientDb.schema.OIndexable.Instances._
import co.topl.genusLibrary.orientDb.schema.OTyped.Instances._
import co.topl.genusLibrary.orientDb.schema.{GraphDataEncoder, VertexSchema}
import com.orientechnologies.orient.core.metadata.schema.OType
import co.topl.brambl.common.ContainsImmutable.instances.ioTransactionImmutable

object SchemaIoTransaction {

  /**
   * IoTransaction model:
   * @see https://github.com/Topl/protobuf-specs/blob/main/proto/brambl/models/transaction/io_transaction.proto
   */
  object Field {
    val SchemaName = "Transaction"
    val TransactionId = "transactionId"
    val Transaction = "transaction"
    val Size = "size"
    val TransactionIndex = "transactionIdIndex"
    val IsReward = "isReward"
  }

  private[genusLibrary] def size(ioTransaction: IoTransaction): Long =
    ioTransactionImmutable.immutableBytes(ioTransaction).value.size

  def make(): VertexSchema[IoTransaction] =
    VertexSchema.create(
      Field.SchemaName,
      GraphDataEncoder[IoTransaction]
        // transactionID is not stored in a transaction, but computed
        .withProperty(
          Field.TransactionId,
          _.id.value.toByteArray,
          mandatory = false,
          readOnly = true,
          notNull = true
        )
        .withProperty(Field.Transaction, _.toByteArray, mandatory = false, readOnly = false, notNull = true)
        .withProperty(
          Field.Size,
          ioTransaction => java.lang.Long.valueOf(size(ioTransaction)),
          mandatory = true,
          readOnly = true,
          notNull = true
        )
        .withProperty(
          Field.IsReward,
          _ => java.lang.Boolean.valueOf(false),
          mandatory = false,
          readOnly = false,
          notNull = true
        )
        .withIndex[IoTransaction](Field.TransactionIndex, Field.TransactionId)
        .withLink(SchemaBlockHeader.Field.BlockId, OType.LINK, SchemaBlockHeader.Field.SchemaName),
      v => IoTransaction.parseFrom(v(Field.Transaction): Array[Byte])
    )

}
