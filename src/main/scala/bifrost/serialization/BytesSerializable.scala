package bifrost.serialization

import bifrost.utils.serialization.BifrostSerializer

trait BytesSerializable extends Serializable {

  type M >: this.type <: BytesSerializable

  lazy val bytes: Array[Byte] = serializer.toBytes(this)

  def serializer: BifrostSerializer[M]
}
