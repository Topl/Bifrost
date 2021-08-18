package co.topl.models

import io.estatico.newtype.macros.newtype

@newtype case class TypedBytes(allBytes: Bytes) {
  def typePrefix: TypePrefix = allBytes.head
  def dataBytes: Bytes = allBytes.tail
}

object TypedBytes {

  def apply(prefix: TypePrefix, dataBytes: Bytes): TypedBytes =
    TypedBytes(dataBytes.prepended(prefix))
}
