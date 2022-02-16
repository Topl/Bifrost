package co.topl.typeclasses

import co.topl.models.{Bytes, TypedBytes}

trait IdentityOps {

  implicit def byteByteVectorTupleAsTypedBytes(t: (Byte, Bytes)): TypedBytes =
    TypedBytes(t._1, t._2)

  implicit class ByteByteVectorTupleOps(t: (Byte, Bytes)) {
    def asTypedBytes: TypedBytes = TypedBytes(t._1, t._2)
  }
}

object IdentityOps extends IdentityOps
