package co.topl.codecs.bytes

import co.topl.models._

object BasicCodecs {
  implicit val typedIdentifierCodec: ByteCodec[TypedIdentifier] = ???
  implicit val taktikosAddressCodec: ByteCodec[TaktikosAddress] = ???
  implicit val blockCodec: ByteCodec[Block] = ???
  implicit val transactionCodec: ByteCodec[Transaction] = ???
  implicit val boxCodec: ByteCodec[Box] = ???
}
