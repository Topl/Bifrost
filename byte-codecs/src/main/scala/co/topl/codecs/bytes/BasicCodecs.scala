package co.topl.codecs.bytes

import co.topl.codecs.bytes.ByteCodec.ops._
import co.topl.models.Proofs.Signature
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Length, Ratio, Sized}

object BasicCodecs {

  implicit def strictSizedBytesCodec[L <: Length](implicit l: L): ByteCodec[Sized.Strict[Bytes, L]] =
    new ByteCodec[Sized.Strict[Bytes, L]] {
      def encode(t: Sized.Strict[Bytes, L], writer: Writer): Unit = writer.putBytes(t.data.toArray)

      def decode(reader: Reader): Sized.Strict[Bytes, L] = Sized.strictUnsafe(Bytes(reader.getBytes(l.value)))
    }

  implicit def strictSizedTypedBytesCodec[L <: Length](implicit l: L): ByteCodec[Sized.Strict[TypedBytes, L]] =
    new ByteCodec[Sized.Strict[TypedBytes, L]] {
      def encode(t: Sized.Strict[TypedBytes, L], writer: Writer): Unit = writer.putBytes(t.data.allBytes.toArray)

      def decode(reader: Reader): Sized.Strict[TypedBytes, L] =
        Sized.strictUnsafe(TypedBytes(Bytes(reader.getBytes(l.value))))
    }

  implicit def seqCodec[T: ByteCodec]: ByteCodec[Seq[T]] = new ByteCodec[Seq[T]] {

    def encode(t: Seq[T], writer: Writer): Unit = {
      writer.putInt(t.length)
      t.foreach(_.writeBytesTo(writer))
    }

    def decode(reader: Reader): Seq[T] =
      Seq.fill(reader.getInt())(ByteCodec[T].decode(reader))
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

    def encode(t: VerificationKeys.Ed25519, writer: Writer): Unit =
      t.bytes.writeBytesTo(writer)

    def decode(reader: Reader): VerificationKeys.Ed25519 =
      VerificationKeys.Ed25519(ByteCodec[Sized.Strict[Bytes, VerificationKeys.Ed25519.Length]].decode(reader))

  }

  implicit val privateKeyEd25519Codec: ByteCodec[SecretKeys.Ed25519] = new ByteCodec[SecretKeys.Ed25519] {
    def encode(t: SecretKeys.Ed25519, writer: Writer): Unit = ???

    def decode(reader: Reader): SecretKeys.Ed25519 = ???
  }

  implicit val publicKeyExtendedEd25519Codec: ByteCodec[VerificationKeys.ExtendedEd25519] =
    new ByteCodec[VerificationKeys.ExtendedEd25519] {

      def encode(t: VerificationKeys.ExtendedEd25519, writer: Writer): Unit = {
        t.ed25519.writeBytesTo(writer)
        t.chainCode.writeBytesTo(writer)
      }

      def decode(reader: Reader): VerificationKeys.ExtendedEd25519 =
        VerificationKeys.ExtendedEd25519(
          ByteCodec[VerificationKeys.Ed25519].decode(reader),
          ByteCodec[Sized.Strict[Bytes, VerificationKeys.ExtendedEd25519.ChainCodeLength]].decode(reader)
        )
    }

  implicit val privateKeyExtendedEd25519Codec: ByteCodec[SecretKeys.ExtendedEd25519] =
    new ByteCodec[SecretKeys.ExtendedEd25519] {
      def encode(t: SecretKeys.ExtendedEd25519, writer: Writer): Unit = ???

      def decode(reader: Reader): SecretKeys.ExtendedEd25519 = ???
    }

  implicit val proofSignatureEd25519Codec: ByteCodec[Proofs.Signature.Ed25519] =
    new ByteCodec[Proofs.Signature.Ed25519] {

      def encode(t: Proofs.Signature.Ed25519, writer: Writer): Unit =
        t.bytes.writeBytesTo(writer)

      def decode(reader: Reader): Proofs.Signature.Ed25519 =
        Signature.Ed25519(ByteCodec[Sized.Strict[Bytes, Signature.Ed25519.Length]].decode(reader))
    }

  implicit val vrfSignatureCodec: ByteCodec[Proofs.Signature.VrfEd25519] =
    new ByteCodec[Signature.VrfEd25519] {

      def encode(t: Signature.VrfEd25519, writer: Writer): Unit =
        t.bytes.writeBytesTo(writer)

      def decode(reader: Reader): Signature.VrfEd25519 =
        Signature.VrfEd25519(ByteCodec[Sized.Strict[Bytes, Signature.VrfEd25519.Length]].decode(reader))
    }

  implicit val vkVrfCodec: ByteCodec[VerificationKeys.VrfEd25519] =
    new ByteCodec[VerificationKeys.VrfEd25519] {

      def encode(t: VerificationKeys.VrfEd25519, writer: Writer): Unit =
        t.bytes.writeBytesTo(writer)

      def decode(reader: Reader): VerificationKeys.VrfEd25519 =
        VerificationKeys.VrfEd25519(ByteCodec[Sized.Strict[Bytes, VerificationKeys.VrfEd25519.Length]].decode(reader))
    }

  implicit val vrfCertificateCodec: ByteCodec[EligibilityCertificate] = new ByteCodec[EligibilityCertificate] {

    override def encode(t: EligibilityCertificate, writer: Writer): Unit = {
      t.vrfNonceSig.writeBytesTo(writer)
      t.vrfTestSig.writeBytesTo(writer)
      t.vkVRF.writeBytesTo(writer)
      writer.putBytes(t.thresholdEvidence.data.allBytes.toArray)
    }

    override def decode(reader: Reader): EligibilityCertificate =
      EligibilityCertificate(
        ByteCodec[Proofs.Signature.VrfEd25519].decode(reader),
        ByteCodec[Proofs.Signature.VrfEd25519].decode(reader),
        ByteCodec[VerificationKeys.VrfEd25519].decode(reader),
        ByteCodec[Evidence].decode(reader),
        ByteCodec[Eta].decode(reader)
      )
  }

  implicit val kesPublicKeyCodec: ByteCodec[VerificationKeys.HdKes] =
    new ByteCodec[VerificationKeys.HdKes] {

      def encode(t: VerificationKeys.HdKes, writer: Writer): Unit = {
        //todo: fix
//      t.xvkM.writeBytesTo(writer)
//      writer.putLong(t.t)
      }

      def decode(reader: Reader): VerificationKeys.HdKes =
        VerificationKeys.HdKes(
          Array(0: Byte)
          //todo: fix
//          ByteCodec[VerificationKeys.ExtendedEd25519].decode(reader)
//          //reader.getLong()
        )
    }

//  implicit val sumProductSignatureCodec: ByteCodec[Proofs.Signature.SumProduct] = new ByteCodec[Signature.SumProduct] {
//
//    def encode(t: Signature.SumProduct, writer: Writer): Unit = {
//      t.ecSignature.writeBytesTo(writer)
//      t.vkK.writeBytesTo(writer)
//      t.witness.writeBytesTo(writer)
//    }
//
//    def decode(reader: Reader): Signature.SumProduct =
//      Signature.SumProduct(
//        ByteCodec[Proofs.Signature.Ed25519].decode(reader),
//        ByteCodec[VerificationKeys.Ed25519].decode(reader),
//        reader.getLong(),
//        ByteCodec[Seq[VerificationKeys.Ed25519]].decode(reader)
//      )
//  }
//
//  implicit val hdKesSignatureCodec: ByteCodec[Proofs.Signature.HdKes] = new ByteCodec[Signature.HdKes] {
//
//    def encode(t: Signature.HdKes, writer: Writer): Unit = {
//      writer.putLong(t.i)
//      t.vkI.writeBytesTo(writer)
//      t.ecSignature.writeBytesTo(writer)
//      t.sigSumJ.writeBytesTo(writer)
//      t.sigSumK.writeBytesTo(writer)
//    }
//
//    def decode(reader: Reader): Signature.HdKes = ???
//  }
//
//  implicit val kesCertificateCodec: ByteCodec[OperationalCertificate] = new ByteCodec[OperationalCertificate] {
//
//    override def encode(t: OperationalCertificate, writer: Writer): Unit = {
//      t.opSig.writeBytesTo(writer)
//      t.xvkM.writeBytesTo(writer)
//      writer.putLong(t.slotR)
//    }
//
//    override def decode(reader: Reader): OperationalCertificate =
//      OperationalCertificate(
//        ByteCodec[Proofs.Signature.HdKes].decode(reader),
//        ByteCodec[VerificationKeys.ExtendedEd25519].decode(reader),
//        reader.getLong()
//      )
//  }
}
