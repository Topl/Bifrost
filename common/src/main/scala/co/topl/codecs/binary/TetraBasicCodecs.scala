package co.topl.codecs.binary

import cats.implicits._
import co.topl.codecs.bytes.{Reader, Writer}
import co.topl.models.BlockHeaderV2.Unsigned
import co.topl.models.Proofs.Knowledge
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.{KesBinaryTree, Length, Ratio, Sized}
import scodec.codecs.discriminated
import scodec.{Attempt, Codec, Err}
import shapeless.{::, HList, HNil}
import scodec.codecs.lazily

trait TetraBasicCodecs {

  implicit def strictSizedBytesCodec[L <: Length](implicit l: L): Codec[Sized.Strict[Bytes, L]] =
    bytesCodec(l.value)
      .exmapc[Sized.Strict[Bytes, L]](t =>
        Attempt.fromEither(
          Sized.strict(Bytes(t)).leftMap(e => Err(s"Invalid length: expected ${l.value} but got ${e.length}"))
        )
      )(t => Attempt.successful(t.data.toArray))

  implicit def strictSizedTypedBytesCodec[L <: Length](implicit l: L): Codec[Sized.Strict[TypedBytes, L]] =
    bytesCodec(l.value)
      .exmapc[Sized.Strict[TypedBytes, L]](t =>
        Attempt.fromEither(
          Sized
            .strict(TypedBytes(Bytes(t)))
            .leftMap(e => Err(s"Invalid length: expected ${l.value} but got ${e.length}"))
        )
      )(t => Attempt.successful(t.data.allBytes.toArray))

  implicit val blockHeaderV2Codec: Codec[BlockHeaderV2] = Codec[BlockHeaderV2] {
    def encode(t: BlockHeaderV2, writer: Writer): Unit = ???

    def decode(reader: Reader): BlockHeaderV2 = ???
  }

  implicit val blockBodyV2Codec: Codec[BlockBodyV2] = Codec[BlockBodyV2] {
    def encode(t: BlockBodyV2, writer: Writer): Unit = ???

    def decode(reader: Reader): BlockBodyV2 = ???
  }

  implicit val blockV1Codec: Codec[BlockV1] = Codec[BlockV1] {
    def encode(t: BlockV1, writer: Writer): Unit = ???

    def decode(reader: Reader): BlockV1 = ???
  }

  implicit val transactionCodec: Codec[Transaction] = Codec[Transaction] {
    def encode(t: Transaction, writer: Writer): Unit = ???

    def decode(reader: Reader): Transaction = ???
  }

  implicit val boxCodec: Codec[Box[_]] = Codec[Box[_]] {
    def encode(t: Box[_], writer: Writer): Unit = ???

    def decode(reader: Reader): Box[_] = ???
  }

  implicit val taktikosAddressCodec: Codec[TaktikosAddress] = {
    ()

    new Codec[TaktikosAddress] {

      override def encode(t: TaktikosAddress, writer: Writer): Unit = {
        t.paymentVKEvidence.writeBytesTo(writer)
        t.poolVK.writeBytesTo(writer)
        t.signature.writeBytesTo(writer)
      }

      override def decode(reader: Reader): TaktikosAddress = ???
    }
  }

  // todo: JAA - consider implications of variable vs. fixed length (BigInt vs. Int128)
  // (I think this is never transmitted so probably safe if we used built in BigInt)

  implicit val bigIntCodec: Codec[BigInt] = arrayCodec[Byte].xmapc(t => BigInt(t))(t => t.toByteArray)

  implicit val ratioCodec: Codec[Ratio] =
    (bigIntCodec :: bigIntCodec)
      .xmapc { case numerator :: denominator :: HNil =>
        Ratio(numerator, denominator)
      } { ratio =>
        HList(
          ratio.numerator,
          ratio.denominator
        )
      }
      .as[Ratio]

  implicit val kesBinaryTreeCodec: Codec[KesBinaryTree] = {
    val kesBinaryTreeEmptyCodec: Codec[KesBinaryTree.Empty] =
      Codec[HList].xmapc { case HNil => KesBinaryTree.Empty() }(_ => HNil).as[KesBinaryTree.Empty]

    val kesBinaryTreeLeafCodec: Codec[KesBinaryTree.SigningLeaf] =
      (arrayCodec[Byte] :: arrayCodec[Byte])
        .xmapc { case sk :: vk :: HNil =>
          KesBinaryTree.SigningLeaf(sk, vk)
        } { t =>
          HList(t.sk, t.vk)
        }
        .as[KesBinaryTree.SigningLeaf]

    val kesBinaryTreeNodeCodec: Codec[KesBinaryTree.MerkleNode] =
      (arrayCodec[Byte] :: arrayCodec[Byte] :: arrayCodec[Byte] :: kesBinaryTreeCodec :: kesBinaryTreeCodec)
        .xmapc { case seed :: witnessLeft :: witnessRight :: left :: right :: HNil =>
          KesBinaryTree.MerkleNode(seed, witnessLeft, witnessRight, left, right)
        } { t =>
          HList(t.seed, t.witnessLeft, t.witnessRight, t.left, t.right)
        }
        .as[KesBinaryTree.MerkleNode]

    lazily[KesBinaryTree] {
      discriminated[KesBinaryTree]
        .by(byteCodec)
        .typecase(KesBinaryTree.emptyTypePrefix, kesBinaryTreeEmptyCodec)
        .typecase(KesBinaryTree.leafTypePrefix, kesBinaryTreeLeafCodec)
        .typecase(KesBinaryTree.nodeTypePrefix, kesBinaryTreeNodeCodec)
    }
  }

  implicit val vkCurve25519Codec: Codec[VerificationKeys.Curve25519] =
    strictSizedBytesCodec[VerificationKeys.Curve25519.Length].xmapc(t => VerificationKeys.Curve25519(t))(t => t.bytes)

  implicit val vkEd25519Codec: Codec[VerificationKeys.Ed25519] =
    strictSizedBytesCodec[VerificationKeys.Ed25519.Length].xmapc(t => VerificationKeys.Ed25519(t))(t => t.bytes)

  implicit val vkExtendedEd25519Codec: Codec[VerificationKeys.ExtendedEd25519] =
    (vkEd25519Codec ::
      strictSizedBytesCodec[SecretKeys.ExtendedEd25519.ChainCodeLength])
      .xmapc { case edVk :: chainCode :: HNil =>
        VerificationKeys.ExtendedEd25519(edVk, chainCode)
      } { extendedVk =>
        HList(
          extendedVk.vk,
          extendedVk.chainCode
        )
      }
      .as[VerificationKeys.ExtendedEd25519]

  implicit val vkVrfCodec: Codec[VerificationKeys.VrfEd25519] =
    strictSizedBytesCodec[VerificationKeys.VrfEd25519.Length].xmapc(t => VerificationKeys.VrfEd25519(t))(t => t.bytes)

  implicit val vkKesSumCodec: Codec[VerificationKeys.KesSum] =
    (strictSizedBytesCodec[VerificationKeys.KesSum.Length] :: intCodec)
      .xmapc { case b :: step :: HNil => VerificationKeys.KesSum(b, step) }(t => HList(t.bytes, t.step))
      .as[VerificationKeys.KesSum]

  implicit val vkKesProductCodec: Codec[VerificationKeys.KesProduct] =
    (strictSizedBytesCodec[VerificationKeys.KesProduct.Length] :: intCodec)
      .xmapc { case b :: step :: HNil => VerificationKeys.KesProduct(b, step) }(t => HList(t.bytes, t.step))
      .as[VerificationKeys.KesProduct]

  implicit val proofSignatureCurve25519: Codec[Proofs.Knowledge.Curve25519] =
    strictSizedBytesCodec[Proofs.Knowledge.Curve25519.Length].xmapc(t => Proofs.Knowledge.Curve25519(t))(_.bytes)

  implicit val proofSignatureEd25519Codec: Codec[Proofs.Knowledge.Ed25519] =
    strictSizedBytesCodec[Proofs.Knowledge.Ed25519.Length].xmapc(t => Proofs.Knowledge.Ed25519(t))(_.bytes)

  implicit val proofSignatureVrfCodec: Codec[Proofs.Knowledge.VrfEd25519] =
    strictSizedBytesCodec[Proofs.Knowledge.VrfEd25519.Length].xmapc(t => Proofs.Knowledge.VrfEd25519(t))(_.bytes)

  implicit val proofSignatureKesSumCodec: Codec[Proofs.Knowledge.KesSum] =
    (vkEd25519Codec :: proofSignatureEd25519Codec :: seqCodec[
      Sized.Strict[Bytes, Proofs.Knowledge.KesSum.DigestLength]
    ]).xmapc { case vk :: sig :: witness :: HNil =>
      Proofs.Knowledge.KesSum(vk, sig, witness.toVector)
    } { t =>
      HList(
        t.verificationKey,
        t.signature,
        t.witness
      )
    }.as[Proofs.Knowledge.KesSum]

  implicit val proofSignatureKesProductCodec: Codec[Proofs.Knowledge.KesProduct] =
    (proofSignatureKesSumCodec :: proofSignatureKesSumCodec :: strictSizedBytesCodec[
      Proofs.Knowledge.KesProduct.DigestLength
    ]).xmapc { case parentTree :: childTree :: witness :: HNil =>
      Proofs.Knowledge.KesProduct(parentTree, childTree, witness)
    } { t =>
      HList(
        t.superSignature,
        t.subSignature,
        t.subRoot
      )
    }.as[Proofs.Knowledge.KesProduct]

  implicit val secretKeyCurve25519Codec: Codec[SecretKeys.Curve25519] =
    strictSizedBytesCodec[SecretKeys.Curve25519.Length].xmapc(t => SecretKeys.Curve25519(t))(t => t.bytes)

  implicit val secretKeyEd25519Codec: Codec[SecretKeys.Ed25519] =
    strictSizedBytesCodec[SecretKeys.Ed25519.Length].xmapc(t => SecretKeys.Ed25519(t))(t => t.bytes)

  implicit val secretKeyExtendedEd25519Codec: Codec[SecretKeys.ExtendedEd25519] =
    (strictSizedBytesCodec[SecretKeys.ExtendedEd25519.LeftLength] ::
      strictSizedBytesCodec[SecretKeys.ExtendedEd25519.RightLength] ::
      strictSizedBytesCodec[SecretKeys.ExtendedEd25519.ChainCodeLength])
      .xmapc { case left :: right :: chainCode :: HNil =>
        SecretKeys.ExtendedEd25519(left, right, chainCode)
      } { sk =>
        HList(
          sk.leftKey,
          sk.rightKey,
          sk.chainCode
        )
      }
      .as[SecretKeys.ExtendedEd25519]

  implicit val secretKeyVrfCodec: Codec[SecretKeys.VrfEd25519] =
    strictSizedBytesCodec[SecretKeys.VrfEd25519.Length].xmapc(t => SecretKeys.VrfEd25519(t))(t => t.bytes)

  implicit val secretKeyKesSumCodec: Codec[SecretKeys.KesSum] =
    (kesBinaryTreeCodec :: uLongCodec)
      .xmapc { case tree :: offset :: HNil =>
        SecretKeys.KesSum(tree, offset)
      } { sk =>
        HList(
          sk.tree,
          sk.offset
        )
      }
      .as[SecretKeys.KesSum]

  implicit val secretKeyKesProductCodec: Codec[SecretKeys.KesProduct] =
    (kesBinaryTreeCodec :: kesBinaryTreeCodec :: sizedArrayCodec[Byte](32) :: proofSignatureKesSumCodec :: uLongCodec)
      .xmapc { case parent :: child :: seed :: childSig :: offset :: HNil =>
        SecretKeys.KesProduct(parent, child, seed, childSig, offset)
      } { t =>
        HList(t.superTree, t.subTree, t.nextSubSeed, t.subSignature, t.offset)
      }
      .as[SecretKeys.KesProduct]

  implicit val vrfCertificateCodec: Codec[EligibilityCertificate] =
    (proofSignatureVrfCodec :: vkVrfCodec :: Codec[Evidence] :: Codec[Eta])
      .xmapc { case proofVrf :: vkVrf :: evidence :: eta :: HNil =>
        EligibilityCertificate(proofVrf, vkVrf, evidence, eta)
      } { t =>
        HList(t.vrfSig, t.vkVRF, t.thresholdEvidence, t.eta)
      }
      .as[EligibilityCertificate]

  implicit val partialOperationalCertificateCodec: Codec[BlockHeaderV2.Unsigned.PartialOperationalCertificate] =
    (vkKesProductCodec :: proofSignatureKesProductCodec :: vkEd25519Codec)
      .xmapc { case vkKes :: proofForChild :: vkChild :: HNil =>
        BlockHeaderV2.Unsigned.PartialOperationalCertificate(vkKes, proofForChild, vkChild)
      } { t =>
        HList(t.parentVK, t.parentSignature, t.childVK)
      }
      .as[BlockHeaderV2.Unsigned.PartialOperationalCertificate]
}

object TetraBasicCodecs extends TetraBasicCodecs
