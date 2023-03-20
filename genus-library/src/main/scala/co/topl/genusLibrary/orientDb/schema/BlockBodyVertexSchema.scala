package co.topl.genusLibrary.orientDb.schema

import co.topl.genusLibrary.orientDb.schema.OrientDbTyped.Instances._
import co.topl.node.models.BlockBody
import com.orientechnologies.orient.core.db.OSharedContextEmbedded
import com.orientechnologies.orient.core.metadata.schema.{OClassEmbedded, OType}

object BlockBodyVertexSchema {

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
//      .withLink(BlockHeaderVertexSchema.Field.BlockId, OType.LINK, new OClassEmbedded(new OSharedContextEmbedded()))
    // There is no index needed for block bodies. They are accessed thru links from block headers and transactions
    ,
    v => BlockBody.parseFrom(v(Field.TransactionIds): Array[Byte])
  )

}
