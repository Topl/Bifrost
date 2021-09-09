package co.topl.codecs.bytes

import co.topl.models._
import co.topl.models.utility.{Ratio, Sized}
import co.topl.models.utility.Lengths._
import co.topl.models.utility.HasLength.implicits._

object BasicCodecs {

  implicit val typedIdentifierCodec: ByteCodec[TypedIdentifier] =
    new ByteCodec[TypedIdentifier] {
      override def encode(t: TypedIdentifier, writer: Writer): Unit = ???

      override def decode(reader: Reader): TypedIdentifier = ???
    }

  implicit val taktikosAddressCodec: ByteCodec[TaktikosAddress] =
    new ByteCodec[TaktikosAddress] {

      override def encode(t: TaktikosAddress, writer: Writer): Unit = {
        writer.putBytes(t.paymentVerificationKeyHash.data.toArray)
        writer.putBytes(t.stakingVerificationKey.data.toArray)
        writer.putBytes(t.signature.data.toArray)
      }

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

  implicit val vrfCertificateCodec: ByteCodec[VrfCertificate] = new ByteCodec[VrfCertificate] {

    override def encode(t: VrfCertificate, writer: Writer): Unit = {
      writer.putBytes(t.vkVRF.ed25519.bytes.data.toArray)
      writer.putBytes(t.nonceProof.bytes.data.toArray)
      writer.putBytes(t.testProof.bytes.data.toArray)
    }

    override def decode(reader: Reader): VrfCertificate =
      VrfCertificate(
        PublicKeys.Vrf(
          PublicKeys.Ed25519(Sized.strict[Bytes, PublicKeys.Ed25519.Length](Bytes(reader.getBytes(32))).toOption.get)
        ), // TODO
        Proofs.Consensus.Nonce(
          Sized.strict[Bytes, Proofs.Consensus.Nonce.Length](Bytes(reader.getBytes(80))).toOption.get
        ), // TODO
        Proofs.Consensus.VrfTest(
          Sized.strict[Bytes, Proofs.Consensus.VrfTest.Length](Bytes(reader.getBytes(80))).toOption.get
        ) // TODO
      )
  }
}
