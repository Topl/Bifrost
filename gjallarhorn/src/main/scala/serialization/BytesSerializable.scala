package serialization

import utils.serialization.GjalSerializer

trait BytesSerializable extends Serializable {

  type M >: this.type <: BytesSerializable

  lazy val bytes: Array[Byte] = serializer.toBytes(this)

  def serializer: GjalSerializer[M]
}
