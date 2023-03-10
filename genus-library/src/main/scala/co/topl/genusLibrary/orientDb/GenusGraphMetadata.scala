package co.topl.genusLibrary.orientDb {

  import co.topl.brambl.models.Evidence
  import co.topl.brambl.models.Identifier
  import co.topl.brambl.models.LockAddress
  import co.topl.brambl.models.TransactionOutputAddress
  import co.topl.brambl.models.box.Box
  import co.topl.brambl.models.transaction.IoTransaction
  import co.topl.codecs.bytes.tetra.instances.ioTransactionAsIoTransactionOps
  import co.topl.consensus.models.BlockHeader
  import co.topl.consensus.models.BlockId
  import co.topl.consensus.models.EligibilityCertificate
  import co.topl.consensus.models.OperationalCertificate
  import co.topl.genus.services.Txo
  import co.topl.genus.services.TxoState
  import co.topl.genusLibrary.GenusException
  import co.topl.genusLibrary.utils.BlockUtils
  import co.topl.node.models.BlockBody
  import com.google.protobuf.ByteString
  import com.orientechnologies.orient.core.metadata.schema.OClass.INDEX_TYPE
  import com.tinkerpop.blueprints.impls.orient.OrientEdgeType
  import com.tinkerpop.blueprints.impls.orient.OrientGraphNoTx
  import com.tinkerpop.blueprints.impls.orient.OrientVertexType
  import quivr.models.Digest
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
    implicit private[genusLibrary] val addressVertexSchema: VertexSchema[LockAddress] =
      VertexSchema.create(
        "LockAddress",
        GraphDataEncoder[LockAddress]
          .withProperty("network", v => java.lang.Integer.valueOf(v.network), _.setNotNull(true))
          .withProperty("ledger", v => java.lang.Integer.valueOf(v.ledger), _.setNotNull(true))
          .withProperty(
            "id",
            _.id match {
              case v: LockAddress.Id.Lock32 => Array[Byte](0) ++ v.value.toByteArray
              case v: LockAddress.Id.Lock64 => Array[Byte](1) ++ v.value.toByteArray
            },
            _.setNotNull(true)
          ),
        v => {
          val idData: Array[Byte] = v("id")
          val id = idData(0) match {
            case 0 => LockAddress.Id.Lock32(Identifier.Lock32.parseFrom(idData.tail))
            case 1 => LockAddress.Id.Lock64(Identifier.Lock64.parseFrom(idData.tail))
          }
          LockAddress(v("network"), v("ledger"), id)
        }
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
            p => p.parentHeaderId.value.toByteArray,
            _.setNotNull(true)
          )(byteArrayOrientDbTypes)
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
            e => eligibilityCertificateToByteArray(e.eligibilityCertificate),
            _.setNotNull(true)
          )(byteArrayOrientDbTypes)
          .withProperty(
            "operationalCertificate",
            o => operationalCertificateToByteArray(o.operationalCertificate),
            _.setNotNull(true)
          )(byteArrayOrientDbTypes)
          .withProperty("metadata", _.metadata.toByteArray, _.setNotNull(false))(byteArrayOrientDbTypes)
          .withProperty("StakingAddress", _.address.toByteArray, _.setNotNull(true))(
            byteArrayOrientDbTypes
          )
          .withIndex("blockHeaderIndex", INDEX_TYPE.UNIQUE, "blockId"),
        v =>
          BlockHeader(
            BlockId(ByteString.copyFrom(v("parentHeaderId"): Array[Byte])),
            v("parentSlot"),
            v("txRoot"),
            v("bloomFilter"),
            v("timestamp"),
            v("height"),
            v("slot"),
            byteArrayToEligibilityCertificate(v("eligibilityCertificate")),
            byteArrayToOperationalCertificate(v("operationalCertificate")),
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
        v => BlockBody.parseFrom(v("transactionIds"))
      )

    implicit private[genusLibrary] val transactionSchema: VertexSchema[IoTransaction] =
      VertexSchema.create(
        name = "Transaction",
        GraphDataEncoder[IoTransaction]
          .withProperty(
            "transactionId",
            t => t.id.toByteArray,
            _.setNotNull(true)
          )(byteArrayOrientDbTypes)
          .withProperty("transaction", _.toByteArray, _.setNotNull(true))(byteArrayOrientDbTypes)
          .withIndex("transactionIdIndex", INDEX_TYPE.UNIQUE, "transactionId"),
        // transactionID is not stored in a transaction, but computed
        v => IoTransaction.parseFrom(v("transaction"))
      )

    implicit private[genusLibrary] val txoSchema: VertexSchema[Txo] =
      VertexSchema.create(
        "TxoState",
        GraphDataEncoder[Txo]
          .withProperty(
            "transactionId",
            _.outputAddress.get.getIoTransaction32.evidence.digest.value.toByteArray,
            _.setNotNull(true)
          )(
            byteArrayOrientDbTypes
          )
          .withProperty(
            "transactionOutputIndex",
            txo => java.lang.Short.valueOf(txo.outputAddress.get.index.toShort),
            _.setNotNull(true)
          )(shortOrientDbTyped)
          // TODO
//          .withProperty("assetLabel", _.assetLabel, _.setNotNull(true))(stringOrientDbTyped)
          .withProperty("box", txo => txo.box.toByteArray)(byteArrayOrientDbTypes)
          .withProperty("state", _.state.toString)(stringOrientDbTyped)
          .withProperty(
            "address",
            _.lockAddress.map(_.getLock32.evidence.digest.value.toByteArray).orNull,
            _.setNotNull(false)
          )(byteArrayOrientDbTypes)
          .withIndex("boxId", INDEX_TYPE.UNIQUE, "transactionId", "transactionOutputIndex")
          .withIndex("assetLabel", INDEX_TYPE.NOTUNIQUE, "assetLabel"),
        v => {
          val transactionIdBytes: Array[Byte] = v("transactionId")
          val transactionId = TransactionOutputAddress.Id.IoTransaction32(
            Identifier.IoTransaction32(Evidence.Sized32(Digest.Digest32(ByteString.copyFrom(transactionIdBytes))))
          )
          val txoAddress = TransactionOutputAddress(0, 0, v("transactionOutputIndex"), transactionId)
          Txo(
            Box.parseFrom(v("box")),
            TxoState.values.find(_.name == v("state")).get,
            Some(txoAddress),
            None // TODO
          )
        }
      )

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
      eligibilityCertificate: EligibilityCertificate
    ): Array[Byte] =
      eligibilityCertificate.toByteArray

    def byteArrayToEligibilityCertificate(a: Array[Byte]): EligibilityCertificate =
      EligibilityCertificate.parseFrom(a)

    def operationalCertificateToByteArray(
      operationalCertificate: OperationalCertificate
    ): Array[Byte] =
      operationalCertificate.toByteArray

    def byteArrayToOperationalCertificate(a: Array[Byte]): OperationalCertificate =
      OperationalCertificate.parseFrom(a)
  }
}
