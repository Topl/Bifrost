package co.topl.codecs.bytes.tetra

import cats.data.{Chain, NonEmptyChain}
import cats.implicits._
import co.topl.codecs.bytes.scodecs._
import co.topl.{models => legacyModels}
import legacyModels._
import legacyModels.utility.HasLength.instances._
import legacyModels.utility.StringDataTypes.Latin1Data
import legacyModels.utility._
import co.topl.proto.{models => protoModels}
import co.topl.consensus.{models => consensusModels}
import scodec.codecs.{discriminated, lazily}
import scodec.{Attempt, Codec, Err}
import shapeless.{::, HList, HNil}
import scala.collection.immutable.ListSet
import com.google.protobuf.ByteString

trait TetraScodecCodecs
    extends TetraScodecPrimitiveCodecs
    with TetraScodecCryptoCodecs
    with TetraScodecVerificationKeyCodecs
    with TetraScodecAddressCodecs
    with TetraScodecProofCodecs
    with TetraScodecSecretKeyCodecs
    with TetraScodecBlockCodecs

/**
 * Use this object or the package object to access all of the codecs from outside of this package.
 */
object TetraScodecCodecs extends TetraScodecCodecs

trait TetraScodecPrimitiveCodecs {

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
          Sized.strict(t).leftMap(e => Err(s"Invalid length: expected ${l.value} but got ${e.length}"))
        )
      )(t => Attempt.successful(t.data))

  implicit def strictSizedTypedBytesCodec[L <: Length](implicit l: L): Codec[Sized.Strict[TypedBytes, L]] =
    bytesCodec(l.value)
      .exmapc[Sized.Strict[TypedBytes, L]](t =>
        Attempt.fromEither(
          Sized
            .strict(TypedBytes(t))
            .leftMap(e => Err(s"Invalid length: expected ${l.value} but got ${e.length}"))
        )
      )(t => Attempt.successful(t.data.allBytes))

  implicit val byteVectorCodec: Codec[Bytes] =
    arrayCodec[Byte].xmap(arr => Bytes(arr), _.toArray)

  implicit val typedBytesCodec: Codec[TypedBytes] =
    (byteCodec :: byteVectorCodec)
      .xmapc { case prefix :: data :: _ =>
        TypedBytes(prefix, data)
      }(t => HList(t.typePrefix, t.dataBytes))

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
    byteArrayCodec(16).exmap(
      bytes => Attempt.fromEither(Sized.max[BigInt, Lengths.`128`.type](BigInt(bytes)).leftMap(e => Err(e.toString))),
      int => {
        val dBytes = int.data.toByteArray
        val padValue: Byte = if (dBytes.head < 0) -1 else 0
        Bytes.fill(16 - dBytes.length)(padValue) ++ Bytes(dBytes)
        Attempt.successful(Array.fill(16 - dBytes.length)(padValue) ++ dBytes)
      }
    )

  implicit val unknownFieldSetCodec: Codec[scalapb.UnknownFieldSet] =
    emptyCodec(scalapb.UnknownFieldSet.empty)

  implicit val protobufByteStringCodec: Codec[ByteString] =
    Codec[Array[Byte]].xmapc(ByteString.copyFrom)(_.toByteArray)

  implicit val int128ProtoCodec: Codec[protoModels.Int128] =
    (protobufByteStringCodec :: unknownFieldSetCodec).as[protoModels.Int128]

  implicit val networkPrefixCodec: Codec[NetworkPrefix] =
    Codec[Byte].xmap(NetworkPrefix(_), _.value)

  implicit val networkPrefixProtoCodec: Codec[protoModels.NetworkPrefix] =
    intCodec.xmap(protoModels.NetworkPrefix(_), _.value)

  implicit val typedEvidenceCodec: Codec[TypedEvidence] =
    (Codec[TypePrefix] :: Codec[Evidence]).as[TypedEvidence]

  implicit val typedEvidenceProtoCodec: Codec[protoModels.TypedEvidence] =
    (intCodec :: Codec[ByteString] :: unknownFieldSetCodec).as[protoModels.TypedEvidence]

  implicit val spendingAddressProtoCodec: Codec[protoModels.SpendingAddress] =
    (optionCodec[protoModels.TypedEvidence] :: unknownFieldSetCodec)
      .as[protoModels.SpendingAddress] // TODO: Checksum like old model?

  implicit val rhoCodec: Codec[Rho] =
    Codec[Sized.Strict[Bytes, Lengths.`64`.type]].xmap(Rho(_), _.sizedBytes)

}

trait TetraScodecCryptoCodecs {
  self: TetraScodecPrimitiveCodecs =>

  implicit val kesBinaryTreeCodec: Codec[KesBinaryTree] = {
    val kesBinaryTreeEmptyCodec: Codec[KesBinaryTree.Empty] =
      emptyCodec(KesBinaryTree.Empty())

    val kesBinaryTreeLeafCodec: Codec[KesBinaryTree.SigningLeaf] =
      (arrayCodec[Byte] :: arrayCodec[Byte])
        .as[KesBinaryTree.SigningLeaf]

    val kesBinaryTreeNodeCodec: Codec[KesBinaryTree.MerkleNode] =
      lazily(
        (arrayCodec[Byte] :: arrayCodec[Byte] :: arrayCodec[Byte] :: kesBinaryTreeCodec :: kesBinaryTreeCodec)
          .as[KesBinaryTree.MerkleNode]
      )

    discriminated[KesBinaryTree]
      .by(byteCodec)
      .typecase(KesBinaryTree.emptyTypePrefix, kesBinaryTreeEmptyCodec)
      .typecase(KesBinaryTree.leafTypePrefix, kesBinaryTreeLeafCodec)
      .typecase(KesBinaryTree.nodeTypePrefix, kesBinaryTreeNodeCodec)
  }
}

trait TetraScodecVerificationKeyCodecs {
  self: TetraScodecPrimitiveCodecs =>

  // TODO Remove after full model replacement
  implicit val vkCurve25519Codec: Codec[VerificationKeys.Curve25519] =
    strictSizedBytesCodec[VerificationKeys.Curve25519.Length]
      .as[VerificationKeys.Curve25519]

  implicit val vkCurve25519ProtoCodec: Codec[protoModels.VerificationKeyCurve25519] =
    (protobufByteStringCodec :: unknownFieldSetCodec)
      .as[protoModels.VerificationKeyCurve25519]

  implicit val vkEd25519Codec: Codec[VerificationKeys.Ed25519] =
    strictSizedBytesCodec[VerificationKeys.Ed25519.Length]
      .as[VerificationKeys.Ed25519]

  implicit val vkEd25519ProtoCodec: Codec[protoModels.VerificationKeyEd25519] =
    (protobufByteStringCodec :: unknownFieldSetCodec)
      .as[protoModels.VerificationKeyEd25519]

  implicit val cryptoVkEd25519Codec: Codec[consensusModels.VerificationKeyEd25519] =
    (protobufByteStringCodec :: unknownFieldSetCodec).as[consensusModels.VerificationKeyEd25519]

  implicit val vkExtendedEd25519Codec: Codec[VerificationKeys.ExtendedEd25519] =
    (vkEd25519Codec :: strictSizedBytesCodec[SecretKeys.ExtendedEd25519.ChainCodeLength])
      .as[VerificationKeys.ExtendedEd25519]

  implicit val vkVrfCodec: Codec[VerificationKeys.VrfEd25519] =
    strictSizedBytesCodec[VerificationKeys.VrfEd25519.Length]
      .as[VerificationKeys.VrfEd25519]

  implicit val consensusVkVrfCodec: Codec[consensusModels.VerificationKeyVrfEd25519] =
    (protobufByteStringCodec :: emptyCodec(scalapb.UnknownFieldSet.empty))
      .as[consensusModels.VerificationKeyVrfEd25519]

  implicit val vkKesSumCodec: Codec[VerificationKeys.KesSum] =
    (strictSizedBytesCodec[VerificationKeys.KesSum.Length] :: intCodec)
      .as[VerificationKeys.KesSum]

  implicit val vkKesProductCodec: Codec[VerificationKeys.KesProduct] =
    (strictSizedBytesCodec[VerificationKeys.KesProduct.Length] :: intCodec)
      .as[VerificationKeys.KesProduct]

  implicit val consensusVkKesProductCodec: Codec[consensusModels.VerificationKeyKesProduct] =
    (
      protobufByteStringCodec ::
        intCodec :: // step
        unknownFieldSetCodec
    ).as[consensusModels.VerificationKeyKesProduct]
}

trait TetraScodecAddressCodecs {
  self: TetraScodecPrimitiveCodecs with TetraScodecVerificationKeyCodecs =>

  // TODO Remove after full model replacement
  implicit val stakingAddressesOperatorCodec: Codec[StakingAddresses.Operator] =
    vkEd25519Codec.as[StakingAddresses.Operator]

  implicit val stakingAddressesOperatorProtoCodec: Codec[protoModels.StakingAddressOperator] =
    (optionCodec(vkEd25519ProtoCodec) :: unknownFieldSetCodec).as[protoModels.StakingAddressOperator]

  // TODO Remove after full model replacement
  implicit val stakingAddressesNonStakingCodec: Codec[StakingAddresses.NonStaking.type] =
    emptyCodec(StakingAddresses.NonStaking)

  implicit val stakingAddressesNonStakingProtoCodec: Codec[protoModels.StakingAddressNonStaking] =
    unknownFieldSetCodec.as[protoModels.StakingAddressNonStaking]

  implicit val stakingAddressCodec: Codec[StakingAddress] =
    discriminated[StakingAddress]
      .by(byteCodec)
      .typecase(0: Byte, stakingAddressesOperatorCodec)
      .typecase(1: Byte, stakingAddressesNonStakingCodec)

  // TODO Ask help how to create a codec with Protobuf Sealed
  implicit val stakingAddressProtoCodec: Codec[protoModels.StakingAddress] =
    discriminated[protoModels.StakingAddress]
      .by(byteCodec)
      .typecase(0: Byte, stakingAddressesOperatorProtoCodec)
      .typecase(1: Byte, stakingAddressesNonStakingProtoCodec)
}

trait TetraScodecSecretKeyCodecs {
  self: TetraScodecPrimitiveCodecs with TetraScodecCryptoCodecs with TetraScodecProofCodecs =>

  implicit val secretKeyEd25519Codec: Codec[SecretKeys.Ed25519] =
    strictSizedBytesCodec[SecretKeys.Ed25519.Length].as[SecretKeys.Ed25519]

  implicit val secretKeyVrfCodec: Codec[SecretKeys.VrfEd25519] =
    strictSizedBytesCodec[SecretKeys.VrfEd25519.Length].as[SecretKeys.VrfEd25519]

  implicit val secretKeyKesSumCodec: Codec[SecretKeys.KesSum] =
    (kesBinaryTreeCodec :: uLongCodec)
      .as[SecretKeys.KesSum]

  implicit val secretKeyKesProductCodec: Codec[SecretKeys.KesProduct] =
    (kesBinaryTreeCodec :: kesBinaryTreeCodec :: sizedArrayCodec[Byte](32) :: proofSignatureKesSumCodec :: uLongCodec)
      .as[SecretKeys.KesProduct]
}

trait TetraScodecProofCodecs {
  self: TetraScodecPrimitiveCodecs with TetraScodecVerificationKeyCodecs =>

  // TODO Remove after full model replacement
  implicit val proofSignatureEd25519Codec: Codec[Proofs.Knowledge.Ed25519] =
    strictSizedBytesCodec[Proofs.Knowledge.Ed25519.Length].as[Proofs.Knowledge.Ed25519]

  implicit val proofSignatureEd25519ProtoCodec: Codec[protoModels.ProofKnowledgeEd25519] =
    (protobufByteStringCodec :: unknownFieldSetCodec).as[protoModels.ProofKnowledgeEd25519]

  implicit val consensusProofSignatureEd25519Codec: Codec[consensusModels.SignatureEd25519] =
    (protobufByteStringCodec :: unknownFieldSetCodec).as[consensusModels.SignatureEd25519]

  implicit val proofSignatureVrfProtoCodec: Codec[protoModels.ProofKnowledgeVrfEd25519] =
    (protobufByteStringCodec :: unknownFieldSetCodec).as[protoModels.ProofKnowledgeVrfEd25519]

  implicit val consensusSignatureVrfEd25519Codec: Codec[consensusModels.SignatureVrfEd25519] =
    (protobufByteStringCodec :: emptyCodec(scalapb.UnknownFieldSet.empty))
      .as[consensusModels.SignatureVrfEd25519]

  // TODO Remove after full model replacement
  implicit val proofSignatureKesSumCodec: Codec[Proofs.Knowledge.KesSum] =
    (vkEd25519Codec :: proofSignatureEd25519Codec :: vectorCodec[
      Sized.Strict[Bytes, Proofs.Knowledge.KesSum.DigestLength]
    ]).as[Proofs.Knowledge.KesSum]

  implicit val proofSignatureKesSumProtoCodec: Codec[protoModels.ProofKnowledgeKesSum] =
    (optionCodec(vkEd25519ProtoCodec) ::
      optionCodec(proofSignatureEd25519ProtoCodec) ::
      seqCodec(protobufByteStringCodec) :: // witness
      unknownFieldSetCodec).as[protoModels.ProofKnowledgeKesSum]

  implicit val consensusProofSignatureKesSumCodec: Codec[consensusModels.SignatureKesSum] =
    (
      cryptoVkEd25519Codec ::
        consensusProofSignatureEd25519Codec ::
        seqCodec(protobufByteStringCodec) ::
        unknownFieldSetCodec
    ).as[consensusModels.SignatureKesSum]

  // TODO Remove after full model replacement
  implicit val proofSignatureKesProductCodec: Codec[Proofs.Knowledge.KesProduct] =
    (proofSignatureKesSumCodec :: proofSignatureKesSumCodec :: strictSizedBytesCodec[
      Proofs.Knowledge.KesProduct.DigestLength
    ]).as[Proofs.Knowledge.KesProduct]

  implicit val proofSignatureKesProductProtoCodec: Codec[protoModels.ProofKnowledgeKesProduct] =
    (optionCodec(proofSignatureKesSumProtoCodec) ::
      optionCodec(proofSignatureKesSumProtoCodec) ::
      protobufByteStringCodec :: // subRoot
      unknownFieldSetCodec).as[protoModels.ProofKnowledgeKesProduct]

  implicit val consensusProofSignatureKesProductCodec: Codec[consensusModels.SignatureKesProduct] =
    (
      consensusProofSignatureKesSumCodec ::
        consensusProofSignatureKesSumCodec ::
        protobufByteStringCodec ::
        unknownFieldSetCodec
    ).as[consensusModels.SignatureKesProduct]

}

trait TetraScodecBlockCodecs {
  self: TetraScodecPrimitiveCodecs
    with TetraScodecVerificationKeyCodecs
    with TetraScodecProofCodecs
    with TetraScodecAddressCodecs =>

  implicit val eligibilityCertificateCodec: Codec[EligibilityCertificate] =
    (proofSignatureVrfCodec :: vkVrfCodec :: Codec[Evidence] :: Codec[Eta])
      .as[EligibilityCertificate]

  implicit val consensusEligibilityCertificateCodec: Codec[consensusModels.EligibilityCertificate] =
    (consensusSignatureVrfEd25519Codec ::
      consensusVkVrfCodec ::
      protobufByteStringCodec :: // thresholdEvidence
      protobufByteStringCodec :: // eta
      emptyCodec(scalapb.UnknownFieldSet.empty))
      .as[consensusModels.EligibilityCertificate]

  implicit val operationalCertificateCodec: Codec[OperationalCertificate] =
    (vkKesProductCodec :: proofSignatureKesProductCodec :: vkEd25519Codec :: proofSignatureEd25519Codec)
      .as[OperationalCertificate]

  implicit val partialOperationalCertificateCodec
    : Codec[legacyModels.BlockHeader.Unsigned.PartialOperationalCertificate] =
    (consensusVkKesProductCodec ::
      consensusProofSignatureKesProductCodec ::
      cryptoVkEd25519Codec)
      .as[legacyModels.BlockHeader.Unsigned.PartialOperationalCertificate]

  implicit val blockHeaderCodec: Codec[legacyModels.BlockHeader] =
    (
      typedBytesCodec ::
        longCodec ::
        Codec[TxRoot] ::
        Codec[BloomFilter] ::
        longCodec ::
        longCodec ::
        longCodec ::
        eligibilityCertificateCodec ::
        operationalCertificateCodec ::
        optionCodec(maxSizedCodec[Latin1Data, Lengths.`32`.type]) ::
        stakingAddressesOperatorCodec
    ).as[legacyModels.BlockHeader]

  implicit val consensusBlockHeaderCodec: Codec[consensusModels.BlockHeader] = (
    consensusBlockIdCodec :: // parentHeaderId
      longCodec :: // parentSlot
      protobufByteStringCodec :: // txRoot
      protobufByteStringCodec :: // bloomFilter
      longCodec :: // timestamp
      longCodec :: // height
      longCodec :: // slot
      consensusEligibilityCertificateCodec ::
      consensusOperationalCertificateCodec ::
      protobufByteStringCodec :: // metadata
      protobufByteStringCodec :: // address
      unknownFieldSetCodec
  ).as[consensusModels.BlockHeader]

  implicit val unsignedBlockHeaderCodec: Codec[legacyModels.BlockHeader.Unsigned] =
    (
      consensusBlockIdCodec ::
        longCodec ::
        protobufByteStringCodec ::
        protobufByteStringCodec ::
        longCodec ::
        longCodec ::
        longCodec ::
        consensusEligibilityCertificateCodec ::
        partialOperationalCertificateCodec ::
        protobufByteStringCodec ::
        protobufByteStringCodec
    ).as[legacyModels.BlockHeader.Unsigned]

}
