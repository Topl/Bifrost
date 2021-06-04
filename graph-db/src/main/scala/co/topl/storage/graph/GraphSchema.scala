package co.topl.storage.graph

import com.orientechnologies.orient.core.metadata.schema.{OClass, OType}

import scala.reflect.ClassTag

case class GraphSchema(nodeSchemas: List[NodeSchema[_]], edgeSchemas: List[EdgeSchema[_, _, _]])

case class Property(name: String, propertyType: OType)
case class Index(name: String, indexType: OClass.INDEX_TYPE, propertyName: String)

trait NodeSchema[T] {
  def name: String
  def properties: List[Property]
  def indices: List[Index]
  def srcEdges: List[EdgeSchema[_, T, _]]
  def destEdges: List[EdgeSchema[_, _, T]]

  def encode: T => Map[String, Any]

  def decode: Decoder => T

}

object NodeSchema {

  def apply[T: ClassTag](
    p:   List[Property] = Nil,
    i:   List[Index] = Nil,
    s:   => List[EdgeSchema[_, T, _]],
    d:   => List[EdgeSchema[_, _, T]],
    enc: T => Map[String, Any],
    dec: Decoder => T
  ): NodeSchema[T] = new NodeSchema[T] {
    override def name: String = implicitly[ClassTag[T]].runtimeClass.getSimpleName

    override def properties: List[Property] = p

    override def indices: List[Index] = i

    override def srcEdges: List[EdgeSchema[_, T, _]] = s

    override def destEdges: List[EdgeSchema[_, _, T]] = d

    override def encode: T => Map[String, Any] = enc

    override def decode: Decoder => T = dec
  }
}

trait EdgeSchema[T, Src, Dest] {
  def name: String
  def properties: List[Property]
  def indices: List[Index]
  def encode: T => Map[String, Any]
  def decode: Decoder => T
  def srcSchema: NodeSchema[Src]
  def destSchema: NodeSchema[Dest]
}

object EdgeSchema {

  def apply[T: ClassTag, Src: NodeSchema, Dest: NodeSchema](
    p:   List[Property] = Nil,
    i:   List[Index] = Nil,
    enc: T => Map[String, Any],
    dec: Decoder => T
  ): EdgeSchema[T, Src, Dest] =
    new EdgeSchema[T, Src, Dest] {
      def name: String = implicitly[ClassTag[T]].runtimeClass.getSimpleName
      def properties: List[Property] = p
      def indices: List[Index] = i
      def encode: T => Map[String, Any] = enc
      def decode: Decoder => T = dec
      def srcSchema: NodeSchema[Src] = implicitly[NodeSchema[Src]]
      def destSchema: NodeSchema[Dest] = implicitly[NodeSchema[Dest]]
    }
}

trait Decoder {
  def apply[T](key: String): T
}
