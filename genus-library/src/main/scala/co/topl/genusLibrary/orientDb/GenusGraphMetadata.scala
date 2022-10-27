package co.topl.genusLibrary.orientDb {

  import co.topl.codecs.bytes.tetra.{TetraIdentifiableInstances, TetraScodecCodecs}
  import co.topl.genusLibrary.GenusException
  import co.topl.models._
  import co.topl.models.utility.Length
  import co.topl.models.utility.Lengths._
  import com.orientechnologies.orient.core.metadata.schema.OClass.INDEX_TYPE
  import com.tinkerpop.blueprints.impls.orient.{OrientEdgeType, OrientGraphNoTx, OrientVertexType}
  import scodec.Codec
  import scodec.bits.BitVector

  /**
   * Metadata describing the schema used for the Genus graph in OrientDB
   */
  class GenusGraphMetadata(val graphNoTx: OrientGraphNoTx) {

    import GenusGraphMetadata._

    val addressVertexType: OrientVertexType = ensureVertexSchemaInitialized(addressVertexSchema)
    val boxStateVertexType: OrientVertexType = ensureVertexSchemaInitialized(boxStateSchema)

    val blockHeaderVertexType: OrientVertexType = ensureVertexSchemaInitialized(blockHeaderSchema)

    val blockBodyVertexType: OrientVertexType = ensureVertexSchemaInitialized(blockBodySchema)
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

    // TODO: Rework to use model classes generated from protobuf definitions rather than those in the models project.

    /////////////////////////////////////////////////////////////////////////////////
    //                                                                             //
    // The following Vals are schemas for nodes and edges.                         //
    //                                                                             //
    /////////////////////////////////////////////////////////////////////////////////

    /**
     * Schema for Address nodes
     */
    private val addressVertexSchema: VertexSchema[TypedEvidence] =
      VertexSchema.create(
        "Address",
        GraphDataEncoder[TypedEvidence]
          .withProperty(
            "typePrefix",
            p => java.lang.Byte.valueOf(p.typePrefix),
            _.setMandatory(true).setReadonly(true).setNotNull(true)
          )
          .withProperty("evidence", _.evidence.data.toArray, _.setMandatory(true).setReadonly(true).setNotNull(true))(
            byteArrayOrientDbTypes
          )
          .withIndex("addressIndex", INDEX_TYPE.UNIQUE, "typePrefix", "evidence"),
        v => TypedEvidence(v("typePrefix"), v("evidence"))
      )

    /**
     * Schema for TxO state vertexes
     * <p>
     * Txo state vertexes have no properties, just links to boxes.
     */
    private val boxStateSchema: VertexSchema[Unit] =
      VertexSchema.create(
        "boxState",
        GraphDataEncoder[Unit],
        _ => ()
      )

    private val blockHeaderSchema: VertexSchema[BlockHeaderV2] =
      VertexSchema.create(
        "BlockHeader",
        GraphDataEncoder[BlockHeaderV2]
          .withProperty(
            "blockId",
            b => {
              val (typePrefix, bytes) = TetraIdentifiableInstances.identifiableBlockHeaderV2.idOf(b)
              typedBytesTupleToByteArray((typePrefix, bytes.toArray))
            },
            _.setNotNull(true)
          )(byteArrayOrientDbTypes)
          .withProperty("parentHeaderId", p => typedBytesToByteArray(p.parentHeaderId), _.setNotNull(true))(
            byteArrayOrientDbTypes
          )
          .withProperty("parentSlot", l => java.lang.Long.valueOf(l.parentSlot), _.setMandatory(false))(
            longOrientDbTyped
          )
          .withProperty("txRoot", _.txRoot.data.toArray, _.setMandatory(false))(byteArrayOrientDbTypes)
          .withProperty("bloomFilter", _.bloomFilter.data.toArray, _.setMandatory(false))(byteArrayOrientDbTypes)
          .withProperty("timestamp", ts => java.lang.Long.valueOf(ts.timestamp), _.setNotNull(true))(longOrientDbTyped)
          .withProperty("height", ht => java.lang.Long.valueOf(ht.height), _.setNotNull(true))(longOrientDbTyped)
          .withProperty("slot", s => java.lang.Long.valueOf(s.slot), _.setNotNull(true))(longOrientDbTyped)
          .withProperty(
            "eligibilityCertificate",
            e => eligibilityCertificateToByteArray(e.eligibilityCertificate),
            _.setNotNull(true)
          )(byteArrayOrientDbTypes)
          .withProperty(
            "operationalCertificate",
            o => operationalCertificateToByteArray(o.operationalCertificate),
            _.setNotNull(true)
          )(byteArrayOrientDbTypes)
          .withProperty("metadata", _.metadata.map(_.data.bytes).orNull, _.setNotNull(false))(byteArrayOrientDbTypes)
          .withProperty("address", s => stakingAddressOperatorToByteArray(s.address), _.setNotNull(true))(
            byteArrayOrientDbTypes
          )
          .withIndex("blockHeaderIndex", INDEX_TYPE.UNIQUE, "blockId"),
        v =>
          BlockHeaderV2(
            byteArrayToTypedBytes(v("parentHeaderId")),
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

    private val blockBodySchema: VertexSchema[BlockBodyV2] =
      VertexSchema.create(
        "BlockBody",
        GraphDataEncoder[BlockBodyV2]
          .withProperty("transactionIds", t => blockBodyV2ToByteArray(t), _ => {})(byteArrayOrientDbTypes),
        // There is no index needed for block bodies. They are accessed thru links from block headers and transactions
        v => byteArrayToBlockBodyV2(v("transactionIds"))
      )

    // TODO this needs to change when we switch to models from proto-buffers
    def byteArrayToBlockBodyV2(a: Array[Byte]): BlockBodyV2 =
      decodeFromByteArray(a, TetraScodecCodecs.blockBodyV2Codec, "BlockBodyV2")

    def blockBodyV2ToByteArray(blockBody: BlockBodyV2): Array[Byte] =
      encodeToByteArray(blockBody, TetraScodecCodecs.blockBodyV2Codec, "BlockBodyV2")

    def typedBytesToByteArray(t: TypedIdentifier): Array[Byte] =
      typedBytesTupleToByteArray((t.typePrefix, t.dataBytes.toArray))

    def byteArrayToTypedBytes(a: Array[Byte]): TypedBytes = {
      val tuple = byteArrayToTypedBytesTuple(a)
      TypedBytes(tuple._1, Bytes(tuple._2))
    }

    def byteArrayToTypedBytesTuple(a: Array[Byte]): (TypePrefix, Array[TypePrefix]) = (a(0), a.drop(1))

    def typedBytesTupleToByteArray(id: (TypePrefix, Array[Byte])): Array[Byte] = {
      val a: Array[Byte] = new Array(1 + id._2.length)
      a(0) = id._1
      Array.copy(id._2, 0, a, 1, id._2.length)
      a
    }

    private def encodeToByteArray[T <: java.io.Serializable](objct: T, codec: Codec[T], typeName: String): Array[Byte] =
      codec
        .encode(objct)
        .mapErr(err => throw GenusException(s"Error encoding $typeName: ${err.messageWithContext}"))
        .require
        .toByteArray

    private def decodeFromByteArray[T <: java.io.Serializable](a: Array[Byte], codec: Codec[T], typeName: String): T =
      codec
        .decode(BitVector(a))
        .mapErr { err =>
          throw GenusException(s"Error decoding $typeName: ${err.messageWithContext}")
        }
        .require
        .value

    val evidenceLength: Length = implicitly[Evidence.Length]

    // No need for a byteArrayToBlockHeaderId because it is computed rather than stored.
    def eligibilityCertificateToByteArray(eligibilityCertificate: EligibilityCertificate): Array[Byte] =
      encodeToByteArray(eligibilityCertificate, TetraScodecCodecs.eligibilityCertificateCodec, "EligibilityCertificate")

    def byteArrayToEligibilityCertificate(a: Array[Byte]): EligibilityCertificate =
      decodeFromByteArray(a, TetraScodecCodecs.eligibilityCertificateCodec, "EligibilityCertificate")

    def operationalCertificateToByteArray(operationalCertificate: OperationalCertificate): Array[Byte] =
      encodeToByteArray(operationalCertificate, TetraScodecCodecs.operationalCertificateCodec, "OperationalCertificate")

    def byteArrayToOperationalCertificate(a: Array[Byte]): OperationalCertificate =
      decodeFromByteArray(a, TetraScodecCodecs.operationalCertificateCodec, "OperationalCertificate")

    def stakingAddressOperatorToByteArray(operator: StakingAddresses.Operator): Array[Byte] =
      encodeToByteArray(operator, TetraScodecCodecs.stakingAddressesOperatorCodec, "Operator")

    def byteArrayToStakingAddressOperator(a: Array[Byte]): StakingAddresses.Operator =
      decodeFromByteArray(a, TetraScodecCodecs.stakingAddressesOperatorCodec, "Operator")
  }
}
