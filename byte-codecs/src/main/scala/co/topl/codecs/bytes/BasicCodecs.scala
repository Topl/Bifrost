package co.topl.codecs.bytes

import co.topl.codecs.bytes.ByteCodec.ops._
import co.topl.models.Proofs.Consensus
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Ratio, Sized}

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

  implicit val publicKeyEd25519Codec: ByteCodec[VerificationKeys.Ed25519] = new ByteCodec[VerificationKeys.Ed25519] {
    def encode(t: VerificationKeys.Ed25519, writer: Writer): Unit = ???

    def decode(reader: Reader): VerificationKeys.Ed25519 = ???
  }

  implicit val privateKeyEd25519Codec: ByteCodec[SecretKeys.Ed25519] = new ByteCodec[SecretKeys.Ed25519] {
    def encode(t: SecretKeys.Ed25519, writer: Writer): Unit = ???

    def decode(reader: Reader): SecretKeys.Ed25519 = ???
  }

  implicit val publicKeyExtendedEd25519Codec: ByteCodec[VerificationKeys.ExtendedEd25519] =
    new ByteCodec[VerificationKeys.ExtendedEd25519] {
      def encode(t: VerificationKeys.ExtendedEd25519, writer: Writer): Unit = ???

      def decode(reader: Reader): VerificationKeys.ExtendedEd25519 = ???
    }

  implicit val privateKeyExtendedEd25519Codec: ByteCodec[SecretKeys.ExtendedEd25519] =
    new ByteCodec[SecretKeys.ExtendedEd25519] {
      def encode(t: SecretKeys.ExtendedEd25519, writer: Writer): Unit = ???

      def decode(reader: Reader): SecretKeys.ExtendedEd25519 = ???
    }

  implicit val proofSignatureEd25519Codec: ByteCodec[Proofs.Signature.Ed25519] =
    new ByteCodec[Proofs.Signature.Ed25519] {
      def encode(t: Proofs.Signature.Ed25519, writer: Writer): Unit = ???

      def decode(reader: Reader): Proofs.Signature.Ed25519 = ???
    }

  implicit val vrfCertificateCodec: ByteCodec[Vrf.Certificate] = new ByteCodec[Vrf.Certificate] {

    override def encode(t: Vrf.Certificate, writer: Writer): Unit = {
      writer.putBytes(t.vkVRF.ed25519.bytes.data.toArray)
      writer.putBytes(t.nonceProof.bytes.data.toArray)
      writer.putBytes(t.testProof.bytes.data.toArray)
    }

    override def decode(reader: Reader): Vrf.Certificate =
      Vrf.Certificate(
        VerificationKeys.Vrf(
          VerificationKeys.Ed25519(
            Sized
              .strictUnsafe(
                Bytes(reader.getBytes(implicitly[VerificationKeys.Ed25519.Length].value))
              )
          )
        ),
        Proofs.Vrf.Nonce(
          Sized
            .strictUnsafe(
              Bytes(reader.getBytes(implicitly[Proofs.Vrf.Nonce.Length].value))
            )
        ),
        Proofs.Vrf.Test(
          Sized
            .strictUnsafe(
              Bytes(reader.getBytes(implicitly[Proofs.Vrf.Test.Length].value))
            )
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

  implicit val kesPublicKeyCodec: ByteCodec[VerificationKeys.Kes] = new ByteCodec[VerificationKeys.Kes] {

    def encode(t: VerificationKeys.Kes, writer: Writer): Unit =
      writer.putBytes(t.bytes.data.toArray)

    def decode(reader: Reader): VerificationKeys.Kes =
      VerificationKeys.Kes(
        Sized
          .strictUnsafe(
            Bytes(reader.getBytes(implicitly[VerificationKeys.Kes.Length].value))
          )
      )
  }

  implicit val kesCertificateCodec: ByteCodec[OperationalCertificate] = new ByteCodec[OperationalCertificate] {

    override def encode(t: OperationalCertificate, writer: Writer): Unit = {
      t.kesProof.writeBytesTo(writer)
      t.mmmProof.writeBytesTo(writer)
    }

    override def decode(reader: Reader): OperationalCertificate =
      OperationalCertificate(
        ByteCodec[VerificationKeys.Kes].decode(reader),
        ByteCodec[VerificationKeys.Ed25519].decode(reader),
        ByteCodec[Proofs.Signature.Ed25519].decode(reader),
        ByteCodec[Proofs.Consensus.MMM].decode(reader)
      )
  }
}
