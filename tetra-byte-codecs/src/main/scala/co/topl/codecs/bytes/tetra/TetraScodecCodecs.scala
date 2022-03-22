package co.topl.codecs.bytes.tetra

import cats.implicits._
import co.topl.codecs.bytes.scodecs._
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility._
import scodec.codecs.{discriminated, lazily}
import scodec.{Attempt, Codec, Err}
import shapeless.{::, HList, HNil}

trait TetraScodecCodecs {

  implicit def maxSizedCodec[T: Codec: HasLength, L <: Length](implicit l: L): Codec[Sized.Max[T, L]] =
    Codec[T]
      .exmapc(t =>
        Attempt.fromEither(
          Sized.max(t).leftMap(e => Err(s"Invalid length: expected ${l.value} but got ${e.length}"))
        )
      )(t => Attempt.successful(t.data))

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

  implicit val typedBytesCodec: Codec[TypedBytes] =
    (byteCodec :: arrayCodec[Byte])
      .xmapc { case prefix :: data :: _ =>
        TypedBytes(prefix, Bytes(data))
      }(t => HList(t.typePrefix, t.dataBytes.toArray))
      .as[TypedBytes]

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

  implicit val latin1DataCodec: Codec[Latin1Data] =
    byteStringCodec.exmapc(byteString =>
      Latin1Data
        .validated(byteString)
        .map(data => Attempt.successful(data))
        .valueOr(errs => Attempt.failure(Err(errs.toString)))
    )(latin1Data => Attempt.successful(latin1Data.value))

  implicit val int128Codec: Codec[Int128] =
    bytesCodec(16).exmap(
      bytes => Attempt.fromEither(Sized.max[BigInt, Lengths.`128`.type](BigInt(bytes)).leftMap(e => Err(e.toString))),
      int => {
        val dBytes = int.data.toByteArray
        val padValue: Byte = if (dBytes.head < 0) -1 else 0
        Bytes.fill(16 - dBytes.length)(padValue) ++ Bytes(dBytes)
        Attempt.successful(Array.fill(16 - dBytes.length)(padValue) ++ dBytes)
      }
    )

  implicit val kesBinaryTreeCodec: Codec[KesBinaryTree] = {
    val kesBinaryTreeEmptyCodec: Codec[KesBinaryTree.Empty] =
      Codec[Unit].xmapc(_ => KesBinaryTree.Empty())(_ => ()).as[KesBinaryTree.Empty]

    val kesBinaryTreeLeafCodec: Codec[KesBinaryTree.SigningLeaf] =
      (arrayCodec[Byte] :: arrayCodec[Byte])
        .xmapc { case sk :: vk :: HNil =>
          KesBinaryTree.SigningLeaf(sk, vk)
        } { t =>
          HList(t.sk, t.vk)
        }
        .as[KesBinaryTree.SigningLeaf]

    val kesBinaryTreeNodeCodec: Codec[KesBinaryTree.MerkleNode] =
      lazily(
        (arrayCodec[Byte] :: arrayCodec[Byte] :: arrayCodec[Byte] :: kesBinaryTreeCodec :: kesBinaryTreeCodec)
          .xmapc { case seed :: witnessLeft :: witnessRight :: left :: right :: HNil =>
            KesBinaryTree.MerkleNode(seed, witnessLeft, witnessRight, left, right)
          } { t =>
            HList(t.seed, t.witnessLeft, t.witnessRight, t.left, t.right)
          }
          .as[KesBinaryTree.MerkleNode]
      )

    discriminated[KesBinaryTree]
      .by(byteCodec)
      .typecase(KesBinaryTree.emptyTypePrefix, kesBinaryTreeEmptyCodec)
      .typecase(KesBinaryTree.leafTypePrefix, kesBinaryTreeLeafCodec)
      .typecase(KesBinaryTree.nodeTypePrefix, kesBinaryTreeNodeCodec)
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

  implicit val eligibilityCertificateCodec: Codec[EligibilityCertificate] =
    (proofSignatureVrfCodec :: vkVrfCodec :: Codec[Evidence] :: Codec[Eta])
      .xmapc { case proofVrf :: vkVrf :: evidence :: eta :: HNil =>
        EligibilityCertificate(proofVrf, vkVrf, evidence, eta)
      } { t =>
        HList(t.vrfSig, t.vkVRF, t.thresholdEvidence, t.eta)
      }
      .as[EligibilityCertificate]

  implicit val operationalCertificateCodec: Codec[OperationalCertificate] =
    (vkKesProductCodec :: proofSignatureKesProductCodec :: vkEd25519Codec :: proofSignatureEd25519Codec)
      .xmapc { case vkKes :: proofForChild :: vkChild :: signatureChild :: HNil =>
        OperationalCertificate(vkKes, proofForChild, vkChild, signatureChild)
      } { t =>
        HList(t.parentVK, t.parentSignature, t.childVK, t.childSignature)
      }
      .as[OperationalCertificate]

  implicit val partialOperationalCertificateCodec: Codec[BlockHeaderV2.Unsigned.PartialOperationalCertificate] =
    (vkKesProductCodec :: proofSignatureKesProductCodec :: vkEd25519Codec)
      .xmapc { case vkKes :: proofForChild :: vkChild :: HNil =>
        BlockHeaderV2.Unsigned.PartialOperationalCertificate(vkKes, proofForChild, vkChild)
      } { t =>
        HList(t.parentVK, t.parentSignature, t.childVK)
      }
      .as[BlockHeaderV2.Unsigned.PartialOperationalCertificate]

  implicit val taktikosAddressCodec: Codec[TaktikosAddress] =
    (strictSizedBytesCodec[Lengths.`32`.type] :: vkEd25519Codec :: proofSignatureEd25519Codec)
      .xmapc { case evidence :: poolVK :: signature :: _ => TaktikosAddress(evidence, poolVK, signature) }(a =>
        HList(a.paymentVKEvidence, a.poolVK, a.signature)
      )

  implicit val blockHeaderV2Codec: Codec[BlockHeaderV2] =
    (typedBytesCodec :: longCodec :: Codec[TxRoot] :: Codec[
      BloomFilter
    ] :: longCodec :: longCodec :: longCodec :: eligibilityCertificateCodec :: operationalCertificateCodec :: optionCodec(
      maxSizedCodec[Latin1Data, Lengths.`32`.type]
    ) :: taktikosAddressCodec)
      .xmapc {
        case parentHeaderId :: parentSlot :: txRoot :: bloomFilter :: timestamp :: height :: slot :: eligibilityCertificate :: operationalCertificate :: metadata :: address :: _ =>
          BlockHeaderV2(
            parentHeaderId,
            parentSlot,
            txRoot,
            bloomFilter,
            timestamp,
            height,
            slot,
            eligibilityCertificate,
            operationalCertificate,
            metadata,
            address
          )
      }(t =>
        HList(
          t.parentHeaderId,
          t.parentSlot,
          t.txRoot,
          t.bloomFilter,
          t.timestamp,
          t.height,
          t.slot,
          t.eligibilityCertificate,
          t.operationalCertificate,
          t.metadata,
          t.address
        )
      )

  implicit val slotDataCodec: Codec[SlotData] = Codec.lazily(???)

  implicit val unsignedBlockHeaderV2Codec: Codec[BlockHeaderV2.Unsigned] =
    (typedBytesCodec :: longCodec :: Codec[TxRoot] :: Codec[
      BloomFilter
    ] :: longCodec :: longCodec :: longCodec :: eligibilityCertificateCodec :: partialOperationalCertificateCodec :: optionCodec(
      maxSizedCodec[Latin1Data, Lengths.`32`.type]
    ) :: taktikosAddressCodec)
      .xmapc {
        case parentHeaderId :: parentSlot :: txRoot :: bloomFilter :: timestamp :: height :: slot :: eligibilityCertificate :: partialOperationalCertificate :: metadata :: address :: _ =>
          BlockHeaderV2.Unsigned(
            parentHeaderId,
            parentSlot,
            txRoot,
            bloomFilter,
            timestamp,
            height,
            slot,
            eligibilityCertificate,
            partialOperationalCertificate,
            metadata,
            address
          )
      }(t =>
        HList(
          t.parentHeaderId,
          t.parentSlot,
          t.txRoot,
          t.bloomFilter,
          t.timestamp,
          t.height,
          t.slot,
          t.eligibilityCertificate,
          t.partialOperationalCertificate,
          t.metadata,
          t.address
        )
      )

  implicit val blockBodyV2Codec: Codec[BlockBodyV2] = Codec.lazily(???)

  implicit val transactionCodec: Codec[Transaction] = Codec.lazily(???)

  implicit val taktikosRegistrationBoxCodec: Codec[Box.Values.TaktikosRegistration] =
    Codec[Proofs.Knowledge.KesProduct].xmap(Box.Values.TaktikosRegistration(_), _.commitment)

  implicit val boxCodec: Codec[Box[_]] = Codec.lazily(???)
}

object TetraScodecCodecs extends TetraScodecCodecs
