package co.topl.genusLibrary.orientDb

import co.topl.codecs.bytes.tetra.TetraIdentifiableInstances
import co.topl.models._
import com.orientechnologies.orient.core.metadata.schema.OClass.INDEX_TYPE
import com.tinkerpop.blueprints.impls.orient.{OrientEdgeType, OrientGraphNoTx, OrientVertexType}
import scodec.bits.ByteVector

/**
 * Metadata describing the schema used for the Genus graph in OrientDB
 */
class GenusGraphMetadata(val graphNoTx: OrientGraphNoTx) {
  import GenusGraphMetadata._

  val addressVertexType: OrientVertexType = ensureVertexSchemaInitialized(addressVertexSchema)
  val boxStateVertexType: OrientVertexType = ensureVertexSchemaInitialized(boxStateSchema)

  val blockHeaderVertexType: OrientVertexType = ensureVertexSchemaInitialized(blockHeaderSchema)

  val blockBodyVertexType: OrientVertexType = graphNoTx.createVertexType("BlockBody")
  val boxVertexType: OrientVertexType = graphNoTx.createVertexType("Box")
  val transactionVertexType: OrientVertexType = graphNoTx.createVertexType("Transaction")

  val currentBoxStateEdgeType: OrientEdgeType = graphNoTx.createEdgeType("CurrentBoxState")
  val prevToNextBoxStateEdgeType: OrientEdgeType = graphNoTx.createEdgeType("PrevToNext")
  val stateToBoxEdgeType: OrientEdgeType = graphNoTx.createEdgeType("StateToBox")
  val inputEdgeType: OrientEdgeType = graphNoTx.createEdgeType("Input")
  val outputEdgeType: OrientEdgeType = graphNoTx.createEdgeType("Output")

  def ensureVertexSchemaInitialized(vertexSchema: VertexSchema[_]): OrientVertexType =
    Option(graphNoTx.getVertexType(vertexSchema.name)).getOrElse {
      val vertexType = graphNoTx.createVertexType(vertexSchema.name)
      vertexSchema.properties.foreach(property =>
        property.propertyAttributeSetter({
          val vertexProperty = vertexType.createProperty(property.name, property.propertyType)
          vertexProperty.setReadonly(true).setMandatory(true)
          vertexProperty
        })
      )
      vertexSchema.indices.foreach(index =>
        vertexType.createIndex(index.name, index.indexType, index.propertyNames: _*)
      )
      vertexType
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
        .withProperty(
          "typePrefix",
          p => java.lang.Byte.valueOf(p.typePrefix),
          _.setMandatory(true).setReadonly(true).setNotNull(true)
        )
        .withProperty("evidence", _.evidence.data, _.setMandatory(true).setReadonly(true).setNotNull(true))
        .withIndex("addressIndex", INDEX_TYPE.UNIQUE, "typePrefix", "evidence"),
      v => TypedEvidence(v("typePrefix"), v("evidence"))
    )

  // Box state vertexes have no properties, just links to boxes.
  private val boxStateSchema: VertexSchema[Unit] =
    VertexSchema.create(
      "boxState",
      GraphDataEncoder[Unit],
      v => ()
    )

  private val blockHeaderSchema: VertexSchema[BlockHeaderV2] = {
    // TODO this needs to change when we switch to models from protobufs, because that is just 32 bytes
    def typedBytesToByteArray(t: TypedIdentifier): Array[Byte] =
      typesBytesTupleToByteArray((t.typePrefix, t.dataBytes.toArray))
    def byteArrayToTypedBytes(a: Array[Byte]): TypedEvidence =
      ???

    def typesBytesTupleToByteArray(id: (Byte, Array[Byte])): Array[Byte] = {
      val a: Array[Byte] = new Array(1 + id._2.length)
      a(0) = id._1
      Array.copy(id._2, 0, a, 1, id._2.length)
      a
    }
    // No need for a byteArrayToBlockHeaderId because it is computed rather than stored.

    def eligibilityCertificateToByteArray(eligibilityCertificate: EligibilityCertificate): Array[Byte] = ???
    def byteArrayToEligibilityCertificate(a: Array[Byte]): EligibilityCertificate = ???

    def operationalCertificateToByteArray(eligibilityCertificate: OperationalCertificate): Array[Byte] = ???
    def byteArrayToOperationalCertificate(a: Array[Byte]): OperationalCertificate = ???

    def stakingAddressOperatorToByteArray(eligibilityCertificate: StakingAddresses.Operator): Array[Byte] = ???
    def byteArrayToStakingAddressOperator(a: Array[Byte]): StakingAddresses.Operator = ???

    VertexSchema.create(
      "BlockHeader",
      GraphDataEncoder[BlockHeaderV2]
        .withProperty(
          "blockId",
          b => {
            val (typePrefix, bytes) = TetraIdentifiableInstances.identifiableBlockHeaderV2.idOf(b)
            typesBytesTupleToByteArray((typePrefix, bytes.toArray))
          },
          _.setNotNull(true)
        )(byteArrayOrientDbTypes)
        .withProperty("parentHeaderId", p => typedBytesToByteArray(p.parentHeaderId), _.setNotNull(true))
        .withProperty("parentSlot", l => java.lang.Long.valueOf(l.parentSlot), _.setMandatory(false))
        .withProperty("txRoot", _.txRoot.data.toArray, _.setMandatory(false))
        .withProperty("bloomFilter", _.bloomFilter.data.toArray, _.setMandatory(false))
        .withProperty("timestamp", ts => java.lang.Long.valueOf(ts.timestamp), _.setNotNull(true))
        .withProperty("height", ht => java.lang.Long.valueOf(ht.height), _.setNotNull(true))
        .withProperty("slot", s => java.lang.Long.valueOf(s.slot), _.setNotNull(true))
        .withProperty(
          "eligibilityCertificate",
          e => eligibilityCertificateToByteArray(e.eligibilityCertificate),
          _.setNotNull(true)
        )
        .withProperty(
          "operationalCertificate",
          o => operationalCertificateToByteArray(o.operationalCertificate),
          _.setNotNull(true)
        )
        .withProperty("metadata", _.metadata.map(_.data.bytes).orNull, _.setNotNull(false))
        .withProperty("address", s => stakingAddressOperatorToByteArray(s.address), _.setNotNull(true))
        .withIndex("blockHeaderIndex", INDEX_TYPE.UNIQUE, "blockId"),
      v =>
        BlockHeaderV2(
          v("parentHeaderId"),
          v("parentSlot"),
          v("txRoot"),
          v("bloomFilter"),
          v("timestamp"),
          v("height"),
          v("slot"),
          v("eligibilityCertificate"),
          v("operationalCertificate"),
          v("metadata"),
          v("address")
        )
    )
  }
}
