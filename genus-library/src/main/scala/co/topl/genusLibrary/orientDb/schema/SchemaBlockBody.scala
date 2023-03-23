package co.topl.genusLibrary.orientDb.schema

import co.topl.genusLibrary.orientDb.schema.OrientDbTyped.Instances._
import co.topl.node.models.BlockBody
import com.orientechnologies.orient.core.metadata.schema.OType

object SchemaBlockBody {

  /**
   * BlockBody model fields:
   * @see https://github.com/Topl/protobuf-specs/blob/main/proto/node/models/block.proto
   */
  object Field {
    val SchemaName = "BlockBody"
    val TransactionIds = "transactionIds"
  }

  def make(): VertexSchema[BlockBody] = VertexSchema.create(
    Field.SchemaName,
    GraphDataEncoder[BlockBody]
      .withProperty(Field.TransactionIds, _.toByteArray, mandatory = false, readOnly = false, notNull = false)
      .withLink(SchemaBlockHeader.Field.BlockId, OType.LINK, SchemaBlockHeader.Field.SchemaName),
    v => BlockBody.parseFrom(v(Field.TransactionIds): Array[Byte])
  )

}
