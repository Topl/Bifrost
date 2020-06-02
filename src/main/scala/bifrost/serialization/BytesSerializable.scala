package bifrost.serialization

import bifrost.utils.serialization.Serializer

trait BytesSerializable extends Serializable {

  type M >: this.type <: BytesSerializable

  lazy val bytes: Array[Byte] = serializer.toBytes(this)

  def serializer: Serializer[M]
}
