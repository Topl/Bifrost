package co.topl.genus.orientDb.instances

import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.{GroupId, LockAddress, SeriesId, TransactionOutputAddress}
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.models.BlockHeader
import co.topl.genus.orientDb.schema.VertexSchema
import co.topl.genus.services.Txo
import co.topl.genus.orientDb.schema.EdgeSchemaInstances._
import co.topl.node.models.BlockBody
import com.tinkerpop.blueprints.{Direction, Vertex}
import com.tinkerpop.blueprints.impls.orient.{OrientGraph, OrientVertex}

import scala.jdk.CollectionConverters._
import scala.language.implicitConversions

/**
 * Metadata describing the schema used for the Genus graph in OrientDB
 */
object VertexSchemaInstances {

  trait Instances {
    val blockHeaderSchema: VertexSchema[BlockHeader] = SchemaBlockHeader.make()
    val blockBodySchema: VertexSchema[BlockBody] = SchemaBlockBody.make()
    val ioTransactionSchema: VertexSchema[IoTransaction] = SchemaIoTransaction.make()
    val lockAddressSchema: VertexSchema[LockAddress] = SchemaLockAddress.make()
    val txoSchema: VertexSchema[Txo] = SchemaTxo.make()
    val groupPolicySchema: VertexSchema[GroupPolicy] = SchemaGroupPolicy.make()
    val seriesPolicySchema: VertexSchema[SeriesPolicy] = SchemaSeriesPolicy.make()
  }

  trait Implicits {
    implicit def orientGraphAsOrientGraphOps(graph: OrientGraph): OrientGraphOps = new OrientGraphOps(graph)
  }
  object instances extends Instances
  object implicits extends Instances with Implicits
}

class OrientGraphOps(val graph: OrientGraph) extends AnyVal {
  import VertexSchemaInstances.instances._

  def addBlockHeader(blockHeader: BlockHeader): OrientVertex =
    graph.addVertex(s"class:${blockHeaderSchema.name}", blockHeaderSchema.encode(blockHeader).asJava)

  def addBody(blockBody: BlockBody): OrientVertex =
    graph.addVertex(s"class:${blockBodySchema.name}", blockBodySchema.encode(blockBody).asJava)

  def addIoTx(ioTx: IoTransaction): OrientVertex =
    graph.addVertex(s"class:${ioTransactionSchema.name}", ioTransactionSchema.encode(ioTx).asJava)

  def addLockAddress(address: LockAddress): OrientVertex =
    graph.addVertex(s"class:${lockAddressSchema.name}", lockAddressSchema.encode(address).asJava)

  def addTxo(txo: Txo): OrientVertex =
    graph.addVertex(s"class:${txoSchema.name}", txoSchema.encode(txo).asJava)

  def addGroupPolicy(groupPolicy: GroupPolicy): OrientVertex =
    graph.addVertex(s"class:${groupPolicySchema.name}", groupPolicySchema.encode(groupPolicy).asJava)

  def addSeriesPolicy(seriesPolicy: SeriesPolicy): OrientVertex =
    graph.addVertex(s"class:${seriesPolicySchema.name}", seriesPolicySchema.encode(seriesPolicy).asJava)

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

  def getGroupPolicy(groupId: GroupId): Option[Vertex] =
    graph
      .getVertices(SchemaGroupPolicy.Field.GroupPolicyId, groupId.value.toByteArray)
      .asScala
      .headOption

  def getSeriesPolicy(seriesId: SeriesId): Option[Vertex] =
    graph
      .getVertices(SchemaSeriesPolicy.Field.SeriesPolicyId, seriesId.value.toByteArray)
      .asScala
      .headOption
}
