package co.topl.storage.graph

import com.orientechnologies.orient.core.metadata.schema.{OClass, OType}

case class GraphSchema(nodeSchemas: Set[NodeSchema[_]], edgeSchemas: Set[EdgeSchema[_, _, _]])

trait NodeSchema[T] {
  def name: String
  def properties: Map[String, OType]
  def indices: Map[String, OClass.INDEX_TYPE]

  def srcEdges: Set[EdgeSchema[_, _, T]]
  def destEdges: Set[EdgeSchema[_, T, _]]
}

trait EdgeSchema[T, Src, Dest] {
  def name: String
  def properties: Map[String, OType]
  def srcSchema: NodeSchema[Src]
  def destSchema: NodeSchema[Dest]
}
