package co.topl.genus.interpreters.orientdb

trait EdgeSchema[T, Src, Dest] {
  def name: String
  def properties: Set[Property]
  def indices: Set[Index]
  def encode(t:          T): Map[String, AnyRef]
  def decode(properties: Map[String, AnyRef]): T
}

object EdgeSchema {

  def apply[T, Src, Dest](implicit schema: EdgeSchema[T, Src, Dest]): EdgeSchema[T, Src, Dest] = schema

  def create[T, Src, Dest](
    name:    String,
    encoder: GraphDataEncoder[T],
    decode:  DecodeHelper => T
  ): EdgeSchema[T, Src, Dest] = {
    val _name = name
    val _decode = decode
    new EdgeSchema[T, Src, Dest] {
      def name: String = _name

      def encode(t: T): Map[String, AnyRef] = encoder.encode(t)

      def decode(properties: Map[String, AnyRef]): T = _decode(new DecodeHelper(properties))

      val properties: Set[Property] = encoder.properties

      val indices: Set[Index] = encoder.indices
    }
  }
}
