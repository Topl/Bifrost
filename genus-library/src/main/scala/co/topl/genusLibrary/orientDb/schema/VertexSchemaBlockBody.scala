package co.topl.genusLibrary.orientDb.schema

import co.topl.genusLibrary.orientDb.schema.OrientDbTyped.Instances._
import co.topl.node.models.BlockBody

object VertexSchemaBlockBody {

  /**
   * Names should be aligned with BlockBody model fields:
   * @see https://github.com/Topl/protobuf-specs/blob/main/proto/node/models/block.proto#L14
   */
  object Field {
    val TransactionIds = "transactionIds"
  }

  def make(): VertexSchema[BlockBody] = VertexSchema.create(
    "BlockBody",
    GraphDataEncoder[BlockBody]
      .withProperty(Field.TransactionIds, blockBody => blockBody.toByteArray, _ => {})
    // There is no index needed for block bodies. They are accessed thru links from block headers and transactions
    ,
    v => BlockBody.parseFrom(v(Field.TransactionIds): Array[Byte])
  )

}
