package co.topl.genusLibrary.orientDb.instances

import co.topl.brambl.models.{LockAddress, TransactionOutputAddress}
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.models.BlockHeader
import co.topl.genus.services.Txo
import co.topl.genusLibrary.orientDb.instances.SchemaCanonicalHead.CanonicalHead
import co.topl.genusLibrary.orientDb.schema.EdgeSchemaInstances.{
  blockHeaderBodyEdge,
  blockHeaderRewardEdge,
  blockHeaderTxIOEdge
}
import co.topl.genusLibrary.orientDb.schema.VertexSchema
import co.topl.node.models.BlockBody
import com.tinkerpop.blueprints.{Direction, Vertex}
import com.tinkerpop.blueprints.impls.orient.{OrientGraph, OrientVertex}

import scala.jdk.CollectionConverters._

/**
 * Metadata describing the schema used for the Genus graph in OrientDB
 */
object VertexSchemaInstances {

  trait Instances {

    implicit class Ops(graph: OrientGraph) {

      def addBlockHeader(blockHeader: BlockHeader): OrientVertex =
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

      def addLockAddress(address: LockAddress): OrientVertex =
        graph.addVertex(s"class:${lockAddressSchema.name}", lockAddressSchema.encode(address).asJava)

      def addTxo(txo: Txo): OrientVertex =
        graph.addVertex(s"class:${txoSchema.name}", txoSchema.encode(txo).asJava)

      def getBlockHeader(blockHeader: BlockHeader): Option[Vertex] =
        graph
          .getVertices(SchemaBlockHeader.Field.BlockId, blockHeader.id.value.toByteArray)
          .asScala
          .headOption

      def getBody(blockHeaderVertex: Vertex): Option[Vertex] =
        blockHeaderVertex.getVertices(Direction.OUT, blockHeaderBodyEdge.label).asScala.headOption

      def getIoTxs(blockHeaderVertex: Vertex): Seq[Vertex] =
        blockHeaderVertex.getVertices(Direction.OUT, blockHeaderTxIOEdge.label).asScala.toSeq ++
        blockHeaderVertex.getVertices(Direction.OUT, blockHeaderRewardEdge.label).asScala.toSeq

      def getTxo(address: TransactionOutputAddress): Option[Vertex] =
        graph
          .getVertices(SchemaTxo.Field.TxoId, address.id.value.toByteArray :+ address.index.toByte)
          .asScala
          .headOption
    }

    private[genusLibrary] val blockHeaderSchema: VertexSchema[BlockHeader] = SchemaBlockHeader.make()
    private[genusLibrary] val blockBodySchema: VertexSchema[BlockBody] = SchemaBlockBody.make()
    private[genusLibrary] val ioTransactionSchema: VertexSchema[IoTransaction] = SchemaIoTransaction.make()
    private[genusLibrary] val canonicalHeadSchema: VertexSchema[CanonicalHead.type] = SchemaCanonicalHead.make()
    private[genusLibrary] val lockAddressSchema: VertexSchema[LockAddress] = SchemaLockAddress.make()
    private[genusLibrary] val txoSchema: VertexSchema[Txo] = SchemaTxo.make()

  }
  object instances extends Instances
}
