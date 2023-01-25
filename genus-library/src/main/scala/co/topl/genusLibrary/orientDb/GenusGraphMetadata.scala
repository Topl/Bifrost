package co.topl.genusLibrary.orientDb {

  import cats.implicits.catsSyntaxOptionId
  import co.topl.codecs.bytes.tetra.{TetraIdentifiableInstances, TetraScodecCodecs}
  import co.topl.genusLibrary.utils.BlockUtils
  import co.topl.genusLibrary.{GenusException, Txo, TxoState}
  import co.topl.consensus.models.BlockHeader
  import co.topl.node.models.BlockBody
  import co.topl.{models => legacyModels}
  import legacyModels._
  import com.google.protobuf.ByteString
  import com.orientechnologies.orient.core.metadata.schema.OClass.INDEX_TYPE
  import com.tinkerpop.blueprints.impls.orient.{OrientEdgeType, OrientGraphNoTx, OrientVertexType}
  import scodec.Codec
  import scodec.bits.BitVector

  /**
   * Metadata describing the schema used for the Genus graph in OrientDB
   */
  class GenusGraphMetadata(val graphNoTx: OrientGraphNoTx) {

    import GenusGraphMetadata._

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // The following fields describe the vertexes and edges that make up the data model.  They will be used by other
    // classes, so they are public
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    val addressVertexType: OrientVertexType = ensureVertexSchemaInitialized(addressVertexSchema)
    val addressStateVertexType: OrientVertexType = ensureVertexSchemaInitialized(addressStateSchema)
    val blockHeaderVertexType: OrientVertexType = ensureVertexSchemaInitialized(blockHeaderSchema)
    val blockBodyVertexType: OrientVertexType = ensureVertexSchemaInitialized(blockBodySchema)
    val txoVertexType: OrientVertexType = ensureVertexSchemaInitialized(txoSchema)
    val transactionVertexType: OrientVertexType = ensureVertexSchemaInitialized(transactionSchema)

    val currentAddressStateEdgeType: OrientEdgeType = ensureEdgeSchemaInitialized(EdgeSchema("CurrentAddressState"))

    val prevToNextAddressStateEdgeType: OrientEdgeType = ensureEdgeSchemaInitialized(
      EdgeSchema("PrevToNextAddressState")
    )

    val addressStateToTxoStateEdgeType: OrientEdgeType = ensureEdgeSchemaInitialized(
      EdgeSchema("AddressStateToTxoState")
    )
    val inputEdgeType: OrientEdgeType = ensureEdgeSchemaInitialized(EdgeSchema("Input"))
    val outputEdgeType: OrientEdgeType = ensureEdgeSchemaInitialized(EdgeSchema("Output"))

    def ensureEdgeSchemaInitialized(edgeSchema: EdgeSchema): OrientEdgeType =
      Option(graphNoTx.getEdgeType(edgeSchema.name)).getOrElse {
        graphNoTx.createEdgeType(edgeSchema.name)
      }

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

  object GenusGraphMetadata extends BlockUtils {
    import OrientDbTyped.Instances._

    // TODO: TSDK-274 Move util methods to BlockUtils
    // TODO: Rework to use model classes generated from protobuf definitions rather than those in the models project.

    /////////////////////////////////////////////////////////////////////////////////
    //                                                                             //
    // The following fields are schemas for nodes and edges.                         //
    //                                                                             //
    /////////////////////////////////////////////////////////////////////////////////

    /**
     * Schema for Address nodes
     */
    implicit private[genusLibrary] val addressVertexSchema: VertexSchema[TypedEvidence] =
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
     * address state vertexes have no properties, just links to txoStates.
     */
    implicit private[genusLibrary] val addressStateSchema: VertexSchema[Unit] =
      VertexSchema.create(
        "AddressState",
        GraphDataEncoder[Unit],
        _ => ()
      )

    implicit private[genusLibrary] val blockHeaderSchema: VertexSchema[BlockHeader] =
      VertexSchema.create(
        "BlockHeader",
        GraphDataEncoder[BlockHeader]
          .withProperty(
            "blockId",
            b => getBlockId(b),
            _.setNotNull(true)
          )(byteArrayOrientDbTypes)
          .withProperty(
            "parentHeaderId",
            p => typedBytesToByteArray(TypedBytes.headerFromBlockId(p.parentHeaderId)),
            _.setNotNull(true)
          )(
            byteArrayOrientDbTypes
          )
          .withProperty("parentSlot", l => java.lang.Long.valueOf(l.parentSlot), _.setMandatory(false))(
            longOrientDbTyped
          )
          .withProperty("txRoot", _.txRoot.toByteArray, _.setMandatory(false))(byteArrayOrientDbTypes)
          .withProperty("bloomFilter", _.bloomFilter.toByteArray, _.setMandatory(false))(byteArrayOrientDbTypes)
          .withProperty("timestamp", ts => java.lang.Long.valueOf(ts.timestamp), _.setNotNull(true))(longOrientDbTyped)
          .withProperty("height", ht => java.lang.Long.valueOf(ht.height), _.setNotNull(true))(longOrientDbTyped)
          .withProperty("slot", s => java.lang.Long.valueOf(s.slot), _.setNotNull(true))(longOrientDbTyped)
          .withProperty(
            "eligibilityCertificate",
            e => e.eligibilityCertificate.map(eligibilityCertificateToByteArray).getOrElse(Array.empty[Byte]),
            _.setNotNull(true)
          )(byteArrayOrientDbTypes)
          .withProperty(
            "operationalCertificate",
            o => o.operationalCertificate.map(operationalCertificateToByteArray).getOrElse(Array.empty[Byte]),
            _.setNotNull(true)
          )(byteArrayOrientDbTypes)
          .withProperty("metadata", _.metadata.toByteArray, _.setNotNull(false))(byteArrayOrientDbTypes)
          .withProperty("StakingAddress", _.address.toByteArray, _.setNotNull(true))(
            byteArrayOrientDbTypes
          )
          .withIndex("blockHeaderIndex", INDEX_TYPE.UNIQUE, "blockId"),
        v =>
          co.topl.consensus.models.BlockHeader(
            co.topl.consensus.models.BlockId(ByteString.copyFrom(v("parentHeaderId"): Array[Byte])).some,
            v("parentSlot"),
            v("txRoot"),
            v("bloomFilter"),
            v("timestamp"),
            v("height"),
            v("slot"),
            byteArrayToEligibilityCertificate(v("eligibilityCertificate")).some,
            byteArrayToOperationalCertificate(v("operationalCertificate")).some,
            v("metadata"),
            v("StakingAddress")
          )
      )

    implicit private[genusLibrary] val blockBodySchema: VertexSchema[BlockBody] =
      VertexSchema.create(
        "BlockBody",
        GraphDataEncoder[BlockBody]
          .withProperty("transactionIds", t => blockBodyToByteArray(t), _ => {})(byteArrayOrientDbTypes),
        // There is no index needed for block bodies. They are accessed thru links from block headers and transactions
        v => byteArrayToBlockBody(v("transactionIds"))
      )

    implicit private[genusLibrary] val transactionSchema: VertexSchema[Transaction] =
      VertexSchema.create(
        name = "Transaction",
        GraphDataEncoder[Transaction]
          .withProperty(
            "transactionId",
            t => {
              val (typePrefix, bytes) = TetraIdentifiableInstances.transactionIdentifiable.idOf(t)
              typedBytesTupleToByteArray((typePrefix, bytes.toArray))
            },
            _.setNotNull(true)
          )(byteArrayOrientDbTypes)
          .withProperty("transaction", transactionToByteArray, _.setNotNull(true))(byteArrayOrientDbTypes)
          .withIndex("transactionIdIndex", INDEX_TYPE.UNIQUE, "transactionId"),
        // transactionID is not stored in a transaction, but computed
        v => byteArrayToTransaction(v("transaction"))
      )

    implicit private[genusLibrary] val txoSchema: VertexSchema[Txo] =
      VertexSchema.create(
        "TxoState",
        GraphDataEncoder[Txo]
          .withProperty("transactionId", _.id.transactionId.dataBytes.toArray, _.setNotNull(true))(
            byteArrayOrientDbTypes
          )
          .withProperty(
            "transactionOutputIndex",
            txo => java.lang.Short.valueOf(txo.id.transactionOutputIndex),
            _.setNotNull(true)
          )(shortOrientDbTyped)
          .withProperty("assetLabel", _.assetLabel, _.setNotNull(true))(stringOrientDbTyped)
          .withProperty("box", txo => boxToByteArray(txo.box))(byteArrayOrientDbTypes)
          .withProperty("state", _.state.toString)(stringOrientDbTyped)
          .withProperty(
            "spendingAddress",
            _.address.map(address => address.typedEvidence.allBytes.toArray).orNull,
            _.setNotNull(false)
          )(byteArrayOrientDbTypes)
          .withIndex("boxId", INDEX_TYPE.UNIQUE, "transactionId", "transactionOutputIndex")
          .withIndex("assetLabel", INDEX_TYPE.NOTUNIQUE, "assetLabel"),
        v => {
          val transactionIdBytes: Array[Byte] = v("transactionId")
          Txo(
            byteArrayToBox(v("box")),
            TxoState.withName(v("state")),
            Box.Id(TypedBytes(Bytes(transactionIdBytes)), v("transactionOutputIndex").asInstanceOf[Short]),
            Option(SpendingAddress(TypedEvidence.fromAllBytes(Bytes(v("spendingAddress").asInstanceOf[Array[Byte]]))))
          )
        }
      )

    // TODO this needs to change when we switch to models from proto-buffers
    def boxToByteArray(box: Box): Array[Byte] =
      encodeToByteArray(box, TetraScodecCodecs.boxCodec, "Box")

    def byteArrayToBox(a: Array[Byte]): Box =
      decodeFromByteArray(a, TetraScodecCodecs.boxCodec, "Box")

    def transactionToByteArray(transaction: Transaction): Array[Byte] =
      encodeToByteArray(transaction, TetraScodecCodecs.transactionCodec, "Transaction")

    def byteArrayToTransaction(a: Array[Byte]): Transaction =
      decodeFromByteArray(a, TetraScodecCodecs.transactionCodec, "Transaction")

    def byteArrayToBlockBody(a: Array[Byte]): BlockBody = BlockBody.parseFrom(a)

    def typedBytesToByteArray(t: TypedIdentifier): Array[Byte] =
      typedBytesTupleToByteArray((t.typePrefix, t.dataBytes.toArray))

    def byteArrayToTypedBytes(a: Array[Byte]): TypedBytes = {
      val tuple = byteArrayToTypedBytesTuple(a)
      TypedBytes(tuple._1, Bytes(tuple._2))
    }

    def byteArrayToTypedBytesTuple(a: Array[Byte]): (TypePrefix, Array[TypePrefix]) = (a(0), a.drop(1))

    private def decodeFromByteArray[T <: java.io.Serializable](a: Array[Byte], codec: Codec[T], typeName: String): T =
      codec
        .decode(BitVector(a))
        .mapErr { err =>
          throw GenusException(s"Error decoding $typeName: ${err.messageWithContext}")
        }
        .require
        .value

    // TODO discuss implementation about decoder and encoder
    // No need for a byteArrayToBlockHeaderId because it is computed rather than stored.
    def eligibilityCertificateToByteArray(
      eligibilityCertificate: co.topl.consensus.models.EligibilityCertificate
    ): Array[Byte] =
      eligibilityCertificate.toByteArray

    def byteArrayToEligibilityCertificate(a: Array[Byte]): co.topl.consensus.models.EligibilityCertificate =
      co.topl.consensus.models.EligibilityCertificate.parseFrom(a)

    def operationalCertificateToByteArray(
      operationalCertificate: co.topl.consensus.models.OperationalCertificate
    ): Array[Byte] =
      operationalCertificate.toByteArray

    def byteArrayToOperationalCertificate(a: Array[Byte]): co.topl.consensus.models.OperationalCertificate =
      co.topl.consensus.models.OperationalCertificate.parseFrom(a)
  }
}
