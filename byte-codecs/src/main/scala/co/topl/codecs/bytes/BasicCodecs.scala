package co.topl.codecs.bytes

import co.topl.models._
import co.topl.models.utility.Ratio

object BasicCodecs {

  implicit val typedIdentifierCodec: ByteCodec[TypedIdentifier] =
    new ByteCodec[TypedIdentifier] {
      override def encode(t: TypedIdentifier, writer: Writer): Unit = ???

      override def decode(reader: Reader): TypedIdentifier = ???
    }

  implicit val taktikosAddressCodec: ByteCodec[TaktikosAddress] =
    new ByteCodec[TaktikosAddress] {
      override def encode(t: TaktikosAddress, writer: Writer): Unit = ???

      override def decode(reader: Reader): TaktikosAddress = ???
    }

  implicit val blockHeaderV2Codec: ByteCodec[BlockHeaderV2] = new ByteCodec[BlockHeaderV2] {
    override def encode(t: BlockHeaderV2, writer: Writer): Unit = ???

    override def decode(reader: Reader): BlockHeaderV2 = ???
  }

  implicit val blockBodyV2Codec: ByteCodec[BlockBodyV2] = new ByteCodec[BlockBodyV2] {
    override def encode(t: BlockBodyV2, writer: Writer): Unit = ???

    override def decode(reader: Reader): BlockBodyV2 = ???
  }

  implicit val blockV1Codec: ByteCodec[BlockV1] = new ByteCodec[BlockV1] {
    override def encode(t: BlockV1, writer: Writer): Unit = ???

    override def decode(reader: Reader): BlockV1 = ???
  }

  implicit val transactionCodec: ByteCodec[Transaction] = new ByteCodec[Transaction] {
    override def encode(t: Transaction, writer: Writer): Unit = ???

    override def decode(reader: Reader): Transaction = ???
  }

  implicit val boxCodec: ByteCodec[Box[_]] = new ByteCodec[Box[_]] {
    override def encode(t: Box[_], writer: Writer): Unit = ???

    override def decode(reader: Reader): Box[_] = ???
  }

  implicit val ratioCodec: ByteCodec[Ratio] = new ByteCodec[Ratio] {

    override def encode(t: Ratio, writer: Writer): Unit = {
      val numeratorBytes = t.numerator.toByteArray
      writer.putInt(numeratorBytes.length)
      writer.putBytes(numeratorBytes)
      val denominatorBytes = t.denominator.toByteArray
      writer.putInt(denominatorBytes.length)
      writer.putBytes(denominatorBytes)
    }

    override def decode(reader: Reader): Ratio =
      Ratio(
        BigInt(reader.getBytes(reader.getInt())),
        BigInt(reader.getBytes(reader.getInt()))
      )
  }
}
