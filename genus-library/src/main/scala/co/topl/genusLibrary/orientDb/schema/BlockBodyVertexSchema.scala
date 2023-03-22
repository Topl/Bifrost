package co.topl.genusLibrary.orientDb.schema

import co.topl.genusLibrary.orientDb.schema.OrientDbTyped.Instances._
import co.topl.node.models.BlockBody
import com.orientechnologies.orient.core.metadata.schema.OType

object BlockBodyVertexSchema {

  /**
   * Names should be aligned with BlockBody model fields:
   * @see https://github.com/Topl/protobuf-specs/blob/main/proto/node/models/block.proto#L14
   */
  object Field {
    val SchemaName = "BlockBody"
    val TransactionIds = "transactionIds"
  }

  def make(): VertexSchema[BlockBody] = VertexSchema.create(
    Field.SchemaName,
    GraphDataEncoder[BlockBody]
      .withProperty(
        Field.TransactionIds,
        blockBody => blockBody.toByteArray,
        mandatory = false,
        readOnly = false,
        notNull = false
      )
      .withLink(BlockHeaderVertexSchema.Field.BlockId, OType.LINK, BlockHeaderVertexSchema.Field.SchemaName),
    v => BlockBody.parseFrom(v(Field.TransactionIds): Array[Byte])
  )

}
