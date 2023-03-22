package co.topl.genusLibrary.orientDb.schema

import co.topl.brambl.models.transaction.IoTransaction
import co.topl.genusLibrary.orientDb.schema.OrientDbTyped.Instances._
import co.topl.codecs.bytes.tetra.instances.ioTransactionAsIoTransactionOps
import co.topl.genusLibrary.orientDb.schema.OrientDbIndexable.Instances._
import com.orientechnologies.orient.core.metadata.schema.OType

object SchemaIoTransaction {

  /**
   * IoTransaction model:
   * @see https://github.com/Topl/protobuf-specs/blob/main/proto/brambl/models/transaction/io_transaction.proto
   */
  object Field {
    val SchemaName = "Transaction"
    val TransactionId = "transactionId"
    val Transaction = "transaction"
    val TransactionIndex = "transactionIdIndex"
  }

  def make(): VertexSchema[IoTransaction] =
    VertexSchema.create(
      Field.SchemaName,
      GraphDataEncoder[IoTransaction]
        // transactionID is not stored in a transaction, but computed
        .withProperty(
          Field.TransactionId,
          _.id.evidence.digest.value.toByteArray,
          mandatory = false,
          readOnly = true,
          notNull = true
        )
        .withProperty(Field.Transaction, _.toByteArray, mandatory = false, readOnly = false, notNull = true)
        .withIndex[IoTransaction](Field.TransactionIndex, Field.TransactionId)
        .withLink(SchemaBlockHeader.Field.BlockId, OType.LINK, SchemaBlockHeader.Field.SchemaName),
      v => IoTransaction.parseFrom(v(Field.Transaction): Array[Byte])
    )

}