package co.topl.genusLibrary.orientDb

import co.topl.models.TypedEvidence
import com.orientechnologies.orient.core.metadata.schema.OClass.INDEX_TYPE
import com.orientechnologies.orient.core.metadata.schema.OType
import com.tinkerpop.blueprints.impls.orient.{OrientEdgeType, OrientGraphNoTx, OrientVertexType}

/**
 * Metadata describing the schema used for the Genus graph in OrientDB
 */
class GenusGraphMetadata(val graphNoTx: OrientGraphNoTx) {
  import GenusGraphMetadata._

  val addressVertexType: OrientVertexType = ensureVertexSchemaInitialized(addressVertexSchema)

  val boxStateVertexType: OrientVertexType = graphNoTx.createVertexType("BoxState")
  // box states have no properties to configure

  val blockHeaderVertexType: OrientVertexType = graphNoTx.createVertexType("BlockHeader")
  configureBlockHeaderVertexType()

  val blockBodyVertexType: OrientVertexType = graphNoTx.createVertexType("BlockBody")
  val boxVertexType: OrientVertexType = graphNoTx.createVertexType("Box")
  val transactionVertexType: OrientVertexType = graphNoTx.createVertexType("Transaction")

  val currentBoxStateEdgeType: OrientEdgeType = graphNoTx.createEdgeType("CurrentBoxState")
  val prevToNextBoxStateEdgeType: OrientEdgeType = graphNoTx.createEdgeType("PrevToNext")
  val stateToBoxEdgeType: OrientEdgeType = graphNoTx.createEdgeType("StateToBox")
  val inputEdgeType: OrientEdgeType = graphNoTx.createEdgeType("Input")
  val outputEdgeType: OrientEdgeType = graphNoTx.createEdgeType("Output")

  def configureBlockHeaderVertexType(): Unit = {
    blockHeaderVertexType
      .createProperty("blockId", OType.INTEGER)
      .setMandatory(true)
      .setReadonly(true)
      .setNotNull(true)
    blockHeaderVertexType.createIndex("blockHeaderIndex", INDEX_TYPE.UNIQUE, "blockId")

    blockHeaderVertexType
      .createProperty("parentHeaderId", OType.INTEGER)
      .setMandatory(false)
      .setReadonly(true)
      .setNotNull(true)

    blockHeaderVertexType
      .createProperty("parentSlot", OType.LONG)
      .setMandatory(false)
      .setReadonly(true)
      .setNotNull(true)

    blockHeaderVertexType
      .createProperty("txRoot", OType.BINARY) // 32 Bytes
      .setMandatory(false)
      .setReadonly(true)
      .setNotNull(false)

    blockHeaderVertexType
      .createProperty("bloomFilter", OType.BINARY) // 256 Bytes
      .setMandatory(false)
      .setReadonly(true)
      .setNotNull(false)

  }

  def ensureVertexSchemaInitialized(vertexSchema: VertexSchema[_]): OrientVertexType = {
    Option(graphNoTx.getVertexType(vertexSchema.name)).getOrElse{
      val vertexType = graphNoTx.createVertexType(vertexSchema.name)
      vertexSchema.properties.foreach(property => vertexType.createProperty(property.name, property.propertyType))
      vertexSchema.indices.foreach(index => vertexType.createIndex(index.name, index.indexType, index.propertyNames: _*))
      vertexType
    }
  }
}

object GenusGraphMetadata {
  /**
   * Regular expression for the base58 representation of typed evidence.
   */
  private val TypedEvidenceRegex = "^[123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz]{33,46}$"

  import OrientDbTyped.Instances._

  // TODO: Rework to use data model classes generated from protobuf definitions rather than those in the models project.
  private val addressVertexSchema: VertexSchema[TypedEvidence] =
    VertexSchema.create(
      "Address",
      GraphDataEncoder[TypedEvidence]
        .withProperty("typePrefix", t => t.typePrefix)
        .withProperty("evidence", t => t.evidence.data)
        .withIndex("addressIndex", INDEX_TYPE.UNIQUE, "typePrefix", "evidence"),
      v =>
        TypedEvidence(v("typePrefix"), v("evidence"))
    )


//  private def configureAddressVertexType(): Unit = {
//    addressVertexType
//      .createProperty("base58Address", OType.STRING)
//      .setMandatory(true)
//      .setReadonly(true)
//      .setNotNull(true)
//      .setRegexp(TypedEvidenceRegex)
//    addressVertexType.createIndex("addressIndex", INDEX_TYPE.UNIQUE, "base58Address")
//  }
}
