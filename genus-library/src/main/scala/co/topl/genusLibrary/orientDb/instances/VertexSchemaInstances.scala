package co.topl.genusLibrary.orientDb.instances

import co.topl.brambl.models.LockId
import co.topl.brambl.models.box.Box
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.{LockAddress, TransactionId}
import co.topl.brambl.syntax._
import co.topl.consensus.models.BlockHeader
import co.topl.genus.services.{Txo, TxoState}
import co.topl.genusLibrary.orientDb.instances.SchemaCanonicalHead.CanonicalHead
import co.topl.genusLibrary.orientDb.schema.OTyped.Instances._
import co.topl.genusLibrary.orientDb.schema.{GraphDataEncoder, VertexSchema}
import co.topl.node.models.BlockBody
import com.google.protobuf.ByteString
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.orient.{OrientGraph, OrientVertex}

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

    }

    private[genusLibrary] val blockHeaderSchema: VertexSchema[BlockHeader] = SchemaBlockHeader.make()
    private[genusLibrary] val blockBodySchema: VertexSchema[BlockBody] = SchemaBlockBody.make()
    private[genusLibrary] val ioTransactionSchema: VertexSchema[IoTransaction] = SchemaIoTransaction.make()
    private[genusLibrary] val canonicalHeadSchema: VertexSchema[CanonicalHead.type] = SchemaCanonicalHead.make()

    // Note, From here to the end, VertexSchemas not tested
    /**
     * Schema for Address nodes
     */
    implicit private[genusLibrary] val addressVertexSchema: VertexSchema[LockAddress] =
      VertexSchema.create(
        "LockAddress",
        GraphDataEncoder[LockAddress]
          .withProperty(
            "network",
            v => java.lang.Integer.valueOf(v.network),
            mandatory = false,
            readOnly = false,
            notNull = true
          )
          .withProperty(
            "ledger",
            v => java.lang.Integer.valueOf(v.ledger),
            mandatory = false,
            readOnly = false,
            notNull = true
          )
          .withProperty(
            "id",
            _.id.value.toByteArray,
            mandatory = false,
            readOnly = false,
            notNull = true
          ),
        v => {
          val id = LockId.parseFrom(v("id"))
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

    implicit private[genusLibrary] val txoSchema: VertexSchema[Txo] =
      VertexSchema.create(
        "TxoState",
        GraphDataEncoder[Txo]
          .withProperty(
            "transactionId",
            _.outputAddress.get.id.value.toByteArray,
            mandatory = false,
            readOnly = false,
            notNull = true
          )(byteArrayOrientDbTypes)
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
            _.lockAddress.map(_.id.value.toByteArray).orNull,
            mandatory = false,
            readOnly = false,
            notNull = false
          )(byteArrayOrientDbTypes)
//          .withIndex("boxId", INDEX_TYPE.UNIQUE, "transactionId", "transactionOutputIndex") // TODO create index type class instance
        // TODO assetLabel was disabled on https://github.com/Topl/Bifrost/pull/2850
        // .withIndex("assetLabel", INDEX_TYPE.NOTUNIQUE, "assetLabel")
        ,
        v => {
          val transactionId = TransactionId(ByteString.copyFrom(v("transactionId"): Array[Byte]))
          val txoAddress = transactionId.outputAddress(0, 0, v("transactionOutputIndex"))
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
