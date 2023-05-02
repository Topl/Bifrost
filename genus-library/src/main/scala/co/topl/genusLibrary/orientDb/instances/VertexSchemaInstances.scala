package co.topl.genusLibrary.orientDb.instances

import co.topl.brambl.models.LockAddress
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.BlockHeader
import co.topl.genusLibrary.orientDb.instances.SchemaCanonicalHead.CanonicalHead
import co.topl.genusLibrary.orientDb.schema.VertexSchema
import co.topl.node.models.BlockBody
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

      def addAddress(address: LockAddress): OrientVertex =
        graph.addVertex(s"class:${lockAddressSchema.name}", lockAddressSchema.encode(address).asJava)

    }

    private[genusLibrary] val blockHeaderSchema: VertexSchema[BlockHeader] = SchemaBlockHeader.make()
    private[genusLibrary] val blockBodySchema: VertexSchema[BlockBody] = SchemaBlockBody.make()
    private[genusLibrary] val ioTransactionSchema: VertexSchema[IoTransaction] = SchemaIoTransaction.make()
    private[genusLibrary] val canonicalHeadSchema: VertexSchema[CanonicalHead.type] = SchemaCanonicalHead.make()
    private[genusLibrary] val lockAddressSchema: VertexSchema[LockAddress] = SchemaLockAddress.make()

  }
  object instances extends Instances
}
