package co.topl.storage.graph

import com.orientechnologies.orient.core.metadata.schema.{OClass, OType}

import scala.reflect.ClassTag
import scala.util.Try

/**
 * Represents the domain of all node and edge types in a graph
 * @param nodeSchemas All of the node schemas used in this graph
 * @param edgeSchemas All of the edge schemas used in this graph
 */
case class GraphSchema(nodeSchemas: List[NodeSchema[_]], edgeSchemas: List[EdgeSchema[_, _, _]])

/**
 * Represents a class name, properties, indices, and codec for a Node in a Graph Database
 * @tparam T the represented type
 */
trait NodeSchema[T] {
  def name: String
  def properties: List[Property]
  def indices: List[Index]

  def encode: T => Map[String, Any]

  def decode: Decoder[T]
}

object NodeSchema {

  def apply[T: ClassTag](
    p:   List[Property] = Nil,
    i:   List[Index] = Nil,
    enc: T => Map[String, Any],
    dec: Decoder[T]
  ): NodeSchema[T] = new NodeSchema[T] {
    override def name: String = implicitly[ClassTag[T]].runtimeClass.getSimpleName

    override def properties: List[Property] = p

    override def indices: List[Index] = i

    override def encode: T => Map[String, Any] = enc

    override def decode: Decoder[T] = dec
  }
}

case class RawNode(className: String, properties: Map[String, Any])

object RawNode {

  implicit val nodeSchema: NodeSchema[RawNode] =
    new NodeSchema[RawNode] {
      import Decoder._
      override def name: String = ???

      override def properties: List[Property] = ???

      override def indices: List[Index] = ???

      override def encode: RawNode => Map[String, Any] = _.properties

      override def decode: Decoder[RawNode] = props => RawNode(props.getTyped[String]("@class").getOrElse(""), props)
    }
}

/**
 * Represents a class name, properties, indices, codec, src/in Node schema, and dest/out Node schema for an Edge in a Graph Database
 * @tparam T The represented (Edge) type
 * @tparam Src The represented src/in Node type
 * @tparam Dest The represented dest/out Node type
 */
trait EdgeSchema[T, Src, Dest] {
  def name: String
  def properties: List[Property]
  def indices: List[Index]
  def encode: T => Map[String, Any]
  def decode: Decoder[T]
  def srcSchema: NodeSchema[Src]
  def destSchema: NodeSchema[Dest]
}

object EdgeSchema {

  def apply[T: ClassTag, Src: NodeSchema, Dest: NodeSchema](
    p:   List[Property] = Nil,
    i:   List[Index] = Nil,
    enc: T => Map[String, Any],
    dec: Decoder[T]
  ): EdgeSchema[T, Src, Dest] =
    new EdgeSchema[T, Src, Dest] {
      def name: String = implicitly[ClassTag[T]].runtimeClass.getSimpleName
      def properties: List[Property] = p
      def indices: List[Index] = i
      def encode: T => Map[String, Any] = enc
      def decode: Decoder[T] = dec
      def srcSchema: NodeSchema[Src] = implicitly[NodeSchema[Src]]
      def destSchema: NodeSchema[Dest] = implicitly[NodeSchema[Dest]]
    }
}

/**
 * Represents an individual piece of data
 * @param name The name of the property
 * @param propertyType The datatype of the property
 */
case class Property(name: String, propertyType: OType)

/**
 * Represents a reference to a property that should be indexed to improve query performance
 * @param name The name of the index - must be unique within a graph
 * @param indexType The type of the index
 * @param propertyName The name of the property on the entity being indexed
 */
case class Index(name: String, indexType: OClass.INDEX_TYPE, propertyName: String)

trait Decoder[T] {
  def apply(properties: Map[String, Any]): T
}

object Decoder {

  implicit class MapOps(properties: Map[String, Any]) {
    def typed[R](key:    String): R = properties(key).asInstanceOf[R]
    def getTyped[R](key: String): Option[R] = properties.get(key).flatMap(v => Try(v.asInstanceOf[R]).toOption)
  }
}
