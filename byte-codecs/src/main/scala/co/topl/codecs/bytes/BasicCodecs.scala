package co.topl.codecs.bytes

import co.topl.models.Proofs.Consensus
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Ratio, Sized}
import ByteCodec.ops._

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

  implicit val vrfCertificateCodec: ByteCodec[Vrf.Certificate] = new ByteCodec[Vrf.Certificate] {

    override def encode(t: Vrf.Certificate, writer: Writer): Unit = {
      writer.putBytes(t.vkVRF.ed25519.bytes.data.toArray)
      writer.putBytes(t.nonceProof.bytes.data.toArray)
      writer.putBytes(t.testProof.bytes.data.toArray)
    }

    override def decode(reader: Reader): Vrf.Certificate =
      Vrf.Certificate(
        PublicKeys.Vrf(
          PublicKeys.Ed25519(
            Sized
              .strictUnsafe(
                Bytes(reader.getBytes(implicitly[PublicKeys.Ed25519.Length].value))
              )
          )
        ),
        Proofs.Consensus.Nonce(
          Sized
            .strictUnsafe(
              Bytes(reader.getBytes(implicitly[Proofs.Consensus.Nonce.Length].value))
            )
        ),
        Proofs.Consensus.VrfTest(
          Sized
            .strictUnsafe(
              Bytes(reader.getBytes(implicitly[Proofs.Consensus.VrfTest.Length].value))
            )
        )
      )
  }

  implicit val kesCertificateProofCodec: ByteCodec[Proofs.Consensus.KesCertificate] =
    new ByteCodec[Proofs.Consensus.KesCertificate] {

      def encode(t: Proofs.Consensus.KesCertificate, writer: Writer): Unit = {
        writer.putBytes(t.signature.data.toArray)
        writer.putBytes(t.extendedPublicKey.data.toArray)
        writer.putBytes(t.chainCode.data.toArray)
      }

      def decode(reader: Reader): Proofs.Consensus.KesCertificate =
        Proofs.Consensus.KesCertificate(
          Sized
            .strictUnsafe(
              Bytes(reader.getBytes(implicitly[Proofs.Consensus.KesCertificate.SignatureLength].value))
            ),
          Sized
            .strictUnsafe(
              Bytes(reader.getBytes(implicitly[Proofs.Consensus.KesCertificate.ExtendedPublicKeyLength].value))
            ),
          Sized
            .strictUnsafe(
              Bytes(reader.getBytes(implicitly[Proofs.Consensus.KesCertificate.ChainCodeLength].value))
            )
        )

    }

  implicit val mmmProofCodec: ByteCodec[Proofs.Consensus.MMM] =
    new ByteCodec[Consensus.MMM] {

      def encode(t: Consensus.MMM, writer: Writer): Unit = {
        writer.putBytes(t.sigi.toArray)
        writer.putBytes(t.sigm.toArray)
        writer.putBytes(t.pki.toArray)
        writer.putLong(t.offset)
        writer.putBytes(t.pkl.toArray)
      }

      def decode(reader: Reader): Consensus.MMM = ???
    }

  implicit val kesPublicKeyCodec: ByteCodec[PublicKeys.Kes] = new ByteCodec[PublicKeys.Kes] {

    def encode(t: PublicKeys.Kes, writer: Writer): Unit = {
      writer.putBytes(t.bytes.data.toArray)
      writer.putLong(t.offset)
    }

    def decode(reader: Reader): PublicKeys.Kes =
      PublicKeys.Kes(
        Sized
          .strictUnsafe(
            Bytes(reader.getBytes(implicitly[PublicKeys.Kes.Length].value))
          ),
        reader.getLong()
      )
  }

  implicit val kesCertificateCodec: ByteCodec[KesCertificate] = new ByteCodec[KesCertificate] {

    override def encode(t: KesCertificate, writer: Writer): Unit = {
      t.kesProof.writeBytesTo(writer)
      t.mmmProof.writeBytesTo(writer)
    }

    override def decode(reader: Reader): KesCertificate =
      KesCertificate(
        ByteCodec[PublicKeys.Kes].decode(reader),
        ByteCodec[Proofs.Consensus.KesCertificate].decode(reader),
        ByteCodec[Proofs.Consensus.MMM].decode(reader)
      )
  }
}
