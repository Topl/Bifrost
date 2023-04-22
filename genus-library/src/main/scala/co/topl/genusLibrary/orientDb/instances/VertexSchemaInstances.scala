package co.topl.genusLibrary.orientDb.instances

import co.topl.brambl.models.box.Box
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.{Address, Evidence, Identifier, TransactionOutputAddress}
import co.topl.consensus.models.BlockHeader
import co.topl.genus.services.{Txo, TxoState}
import co.topl.genusLibrary.orientDb.instances.SchemaCanonicalHead.CanonicalHead
import co.topl.genusLibrary.orientDb.schema.OTyped.Instances._
import co.topl.genusLibrary.orientDb.schema.{GraphDataEncoder, VertexSchema}
import co.topl.node.models.BlockBody
import com.google.protobuf.ByteString
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.orient.{OrientGraph, OrientVertex}
import quivr.models.Digest

import scala.jdk.CollectionConverters._

/**
 * Metadata describing the schema used for the Genus graph in OrientDB
 */
object VertexSchemaInstances {

  trait Instances {

    implicit class Ops(graph: OrientGraph) {

      def addHeader(blockHeader: BlockHeader): OrientVertex =
        graph.addVertex(s"class:${blockHeaderSchema.name}", blockHeaderSchema.encode(blockHeader).asJava)

      def addBody(blockBody: BlockBody): OrientVertex =
        graph.addVertex(s"class:${blockBodySchema.name}", blockBodySchema.encode(blockBody).asJava)

      def addIoTx(ioTx: IoTransaction): OrientVertex =
        graph.addVertex(s"class:${ioTransactionSchema.name}", ioTransactionSchema.encode(ioTx).asJava)

      def addCanonicalHead(blockHeaderVertex: OrientVertex): Vertex =
        // Is expected that Canonical head schema only contains 1 vertex, the head of the chain
        graph.getVerticesOfClass(s"${canonicalHeadSchema.name}").asScala.headOption match {
          case Some(v) =>
            v.setProperty(canonicalHeadSchema.links.head.propertyName, blockHeaderVertex.getId)
            v
          case None =>
            val v =
              graph.addVertex(s"class:${canonicalHeadSchema.name}", canonicalHeadSchema.encode(CanonicalHead).asJava)
            v.setProperty(canonicalHeadSchema.links.head.propertyName, blockHeaderVertex.getId)
            v
        }

      def addAddress(address: Address): OrientVertex =
        graph.addVertex(s"class:${addressSchema.name}", addressSchema.encode(address).asJava)

    }

    private[genusLibrary] val blockHeaderSchema: VertexSchema[BlockHeader] = SchemaBlockHeader.make()
    private[genusLibrary] val blockBodySchema: VertexSchema[BlockBody] = SchemaBlockBody.make()
    private[genusLibrary] val ioTransactionSchema: VertexSchema[IoTransaction] = SchemaIoTransaction.make()
    private[genusLibrary] val canonicalHeadSchema: VertexSchema[CanonicalHead.type] = SchemaCanonicalHead.make()
    private[genusLibrary] val addressSchema: VertexSchema[Address] = SchemaAddress.make()

    // Note, From here to the end, VertexSchemas not tested
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

    implicit private[genusLibrary] val txoSchema: VertexSchema[Txo] =
      VertexSchema.create(
        "TxoState",
        GraphDataEncoder[Txo]
          .withProperty(
            "transactionId",
            _.outputAddress.get.getIoTransaction32.evidence.digest.value.toByteArray,
            mandatory = false,
            readOnly = false,
            notNull = true
          )(
            byteArrayOrientDbTypes
          )
          .withProperty(
            "transactionOutputIndex",
            txo => java.lang.Short.valueOf(txo.outputAddress.get.index.toShort),
            mandatory = false,
            readOnly = false,
            notNull = true
          )(shortOrientDbTyped)
          // TODO, see the index below
//          .withProperty("assetLabel", _.assetLabel, _.setNotNull(true))(stringOrientDbTyped)
          .withProperty("box", txo => txo.box.toByteArray, mandatory = false, readOnly = false, notNull = false)(
            byteArrayOrientDbTypes
          )
          .withProperty("state", _.state.toString, mandatory = false, readOnly = false, notNull = false)(
            stringOrientDbTyped
          )
          .withProperty(
            "address",
            _.lockAddress.map(_.getLock32.evidence.digest.value.toByteArray).orNull,
            mandatory = false,
            readOnly = false,
            notNull = false
          )(byteArrayOrientDbTypes)
//          .withIndex("boxId", INDEX_TYPE.UNIQUE, "transactionId", "transactionOutputIndex") // TODO create index type class instance
        // TODO assetLabel was disabled on https://github.com/Topl/Bifrost/pull/2850
        // .withIndex("assetLabel", INDEX_TYPE.NOTUNIQUE, "assetLabel")
        ,
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
  }
  object instances extends Instances
}
