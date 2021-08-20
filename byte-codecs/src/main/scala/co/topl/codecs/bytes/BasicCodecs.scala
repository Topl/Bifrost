package co.topl.codecs.bytes

import co.topl.models._

object BasicCodecs {
  implicit val typedIdentifierCodec: ByteCodec[TypedIdentifier] = ???
  implicit val taktikosAddressCodec: ByteCodec[TaktikosAddress] = ???
  implicit val blockHeaderV2Codec: ByteCodec[BlockHeaderV2] = ???
  implicit val blockBodyV2Codec: ByteCodec[BlockBodyV2] = ???
  implicit val blockV1Codec: ByteCodec[BlockV1] = ???
  implicit val transactionCodec: ByteCodec[Transaction] = ???
  implicit val boxCodec: ByteCodec[Box] = ???
}
