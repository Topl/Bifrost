package co.topl.genusLibrary.orientDb

import com.orientechnologies.orient.core.metadata.schema.OClass.INDEX_TYPE
import com.orientechnologies.orient.core.metadata.schema.OType
import com.tinkerpop.blueprints.impls.orient.{OrientEdgeType, OrientGraphNoTx, OrientVertexType}

/**
 * Metadata describing the schema used for the Genus graph in OrientDB
 */
class GenusGraphMetadata(val session: OrientGraphNoTx) {
  import GenusGraphMetadata._

  val addressVertexType: OrientVertexType = session.createVertexType("Address")
  configureAddressVertexType()

  val boxStateVertexType: OrientVertexType = session.createVertexType("BoxState")
  // box states have no properties to configure

  val blockHeaderVertexType: OrientVertexType = session.createVertexType("BlockHeader")
  configureBlockHeaderVertexType()

  val blockBodyVertexType: OrientVertexType = session.createVertexType("BlockBody")
  val boxVertexType: OrientVertexType = session.createVertexType("Box")
  val transactionVertexType: OrientVertexType = session.createVertexType("Transaction")

  val currentBoxStateEdgeType: OrientEdgeType = session.createEdgeType("CurrentBoxState")
  val prevToNextBoxStateEdgeType: OrientEdgeType = session.createEdgeType("PrevToNext")
  val stateToBoxEdgeType: OrientEdgeType = session.createEdgeType("StateToBox")
  val inputEdgeType: OrientEdgeType = session.createEdgeType("Input")
  val outputEdgeType: OrientEdgeType = session.createEdgeType("Output")

  private def configureAddressVertexType(): Unit = {
    addressVertexType
      .createProperty("base58Address", OType.STRING)
      .setMandatory(true)
      .setReadonly(true)
      .setNotNull(true)
      .setRegexp(TypedEvidenceRegex)
    addressVertexType.createIndex("addressIndex", INDEX_TYPE.UNIQUE, "base58Address")
  }

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
}

object GenusGraphMetadata {
  private val TypedEvidenceRegex = "^[123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz]{33,46}$"

}
