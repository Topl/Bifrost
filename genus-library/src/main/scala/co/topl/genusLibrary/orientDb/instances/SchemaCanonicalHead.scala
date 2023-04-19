package co.topl.genusLibrary.orientDb.instances

import co.topl.genusLibrary.orientDb.schema.{GraphDataEncoder, VertexSchema}
import com.orientechnologies.orient.core.metadata.schema.OType

object SchemaCanonicalHead {

  case object CanonicalHead

  /**
   * CanonicalHead is represented by 1 vertex, and it is not associated with any mapping model
   */
  object Field {
    val SchemaName = "CanonicalHead"

  }

  def make(): VertexSchema[CanonicalHead.type] = VertexSchema.create(
    Field.SchemaName,
    GraphDataEncoder[CanonicalHead.type]
      .withLink(SchemaBlockHeader.Field.BlockId, OType.LINK, SchemaBlockHeader.Field.SchemaName),
    _ => CanonicalHead
  )
}
