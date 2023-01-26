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
import co.topl.crypto.{models => cryptoModels}
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
    with TetraScodecBoxCodecs
    with TetraScodecPropositionCodecs
    with TetraScodecTransactionCodecs
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

  implicit val spendingAddressCodec: Codec[SpendingAddress] =
    Codec[TypedEvidence].as[SpendingAddress] // TODO: Checksum

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
      Codec[Unit].xmapc(_ => KesBinaryTree.Empty())(_ => ()).as[KesBinaryTree.Empty]

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

  implicit val criptoVkEd25519Codec: Codec[cryptoModels.VerificationKeyEd25519] =
    (protobufByteStringCodec :: unknownFieldSetCodec).as[cryptoModels.VerificationKeyEd25519]

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

  implicit val fullAddressCodec: Codec[FullAddress] =
    (Codec[NetworkPrefix] :: Codec[SpendingAddress] :: Codec[StakingAddress] :: Codec[Proofs.Knowledge.Ed25519])
      .as[FullAddress]

  implicit val fullAddressProtoCodec: Codec[protoModels.FullAddress] =
    (optionCodec[protoModels.NetworkPrefix] ::
      optionCodec[protoModels.SpendingAddress] ::
      Codec[protoModels.StakingAddress] ::
      optionCodec[protoModels.ProofKnowledgeEd25519] ::
      unknownFieldSetCodec)
      .as[protoModels.FullAddress]
}

trait TetraScodecSecretKeyCodecs {
  self: TetraScodecPrimitiveCodecs with TetraScodecCryptoCodecs with TetraScodecProofCodecs =>

  implicit val secretKeyCurve25519Codec: Codec[SecretKeys.Curve25519] =
    strictSizedBytesCodec[SecretKeys.Curve25519.Length].as[SecretKeys.Curve25519]

  implicit val secretKeyEd25519Codec: Codec[SecretKeys.Ed25519] =
    strictSizedBytesCodec[SecretKeys.Ed25519.Length].as[SecretKeys.Ed25519]

  implicit val secretKeyExtendedEd25519Codec: Codec[SecretKeys.ExtendedEd25519] =
    (strictSizedBytesCodec[SecretKeys.ExtendedEd25519.LeftLength] ::
      strictSizedBytesCodec[SecretKeys.ExtendedEd25519.RightLength] ::
      strictSizedBytesCodec[SecretKeys.ExtendedEd25519.ChainCodeLength])
      .as[SecretKeys.ExtendedEd25519]

  implicit val secretKeyVrfCodec: Codec[SecretKeys.VrfEd25519] =
    strictSizedBytesCodec[SecretKeys.VrfEd25519.Length].as[SecretKeys.VrfEd25519]

  implicit val secretKeyKesSumCodec: Codec[SecretKeys.KesSum] =
    (kesBinaryTreeCodec :: uLongCodec)
      .as[SecretKeys.KesSum]

  implicit val secretKeyKesProductCodec: Codec[SecretKeys.KesProduct] =
    (kesBinaryTreeCodec :: kesBinaryTreeCodec :: sizedArrayCodec[Byte](32) :: proofSignatureKesSumCodec :: uLongCodec)
      .as[SecretKeys.KesProduct]
}

trait TetraScodecBoxCodecs {
  self: TetraScodecPrimitiveCodecs with TetraScodecProofCodecs =>

  // TODO Remove after full model replacement
  implicit val boxValuesEmptyCodec: Codec[Box.Values.Empty.type] =
    emptyCodec(Box.Values.Empty)

  implicit val boxValuesEmptyProtoCodec: Codec[protoModels.EmptyBoxValue] =
    unknownFieldSetCodec.as[protoModels.EmptyBoxValue]

  // TODO Remove after full model replacement
  implicit val boxValuesPolyCodec: Codec[Box.Values.Poly] =
    int128Codec.as[Box.Values.Poly]

  implicit val boxValuesPolyProtoCodec: Codec[protoModels.PolyBoxValue] =
    (optionCodec(int128ProtoCodec) :: unknownFieldSetCodec).as[protoModels.PolyBoxValue]

  // TODO Remove after full model replacement
  implicit val boxValuesArbitCodec: Codec[Box.Values.Arbit] =
    int128Codec.as[Box.Values.Arbit]

  implicit val boxValuesArbitProtoCodec: Codec[protoModels.ArbitBoxValue] =
    (optionCodec(int128ProtoCodec) :: unknownFieldSetCodec).as[protoModels.ArbitBoxValue]

  // TODO Remove after full model replacement
  implicit val boxValuesAssetCodec: Codec[Box.Values.AssetV1] =
    (Codec[Int128] :: Codec[Box.Values.AssetV1.Code] :: Codec[Sized.Strict[Bytes, Lengths.`32`.type]] :: Codec[Option[
      Sized.Max[Latin1Data, Lengths.`127`.type]
    ]]).as[Box.Values.AssetV1]

  implicit val boxValuesAssetCodeProtoCodec: Codec[protoModels.AssetV1BoxValue.Code] =
    (optionCodec(spendingAddressProtoCodec) :: protobufByteStringCodec :: unknownFieldSetCodec)
      .as[protoModels.AssetV1BoxValue.Code]

  implicit val boxValuesAssetProtoCodec: Codec[protoModels.AssetV1BoxValue] =
    (optionCodec(int128ProtoCodec) ::
      optionCodec(boxValuesAssetCodeProtoCodec) ::
      protobufByteStringCodec :: // security root
      protobufByteStringCodec :: // metadata
      unknownFieldSetCodec).as[protoModels.AssetV1BoxValue]

  implicit val boxValuesPoolRegistrationCodec: Codec[Box.Values.Registrations.Operator] =
    Codec[Proofs.Knowledge.KesProduct].as[Box.Values.Registrations.Operator]

  implicit val boxValuesPoolRegistrationProtoCodec: Codec[protoModels.OperatorRegistrationBoxValue] =
    (optionCodec(proofSignatureKesProductProtoCodec) :: unknownFieldSetCodec)
      .as[protoModels.OperatorRegistrationBoxValue]

  // TODO Remove after full model replacement
  implicit val boxIdCodec: Codec[Box.Id] =
    (Codec[TypedIdentifier] :: Codec[Short]).as[Box.Id]

  implicit val transactionIdIdProtoCodec: Codec[protoModels.TransactionId] =
    (
      protobufByteStringCodec ::
        unknownFieldSetCodec
    ).as[protoModels.TransactionId]

  implicit val boxIdProtoCodec: Codec[protoModels.Box.Id] =
    (
      optionCodec(transactionIdIdProtoCodec) ::
        intCodec ::
        unknownFieldSetCodec
    ).as[protoModels.Box.Id]

  implicit val boxValueCodec: Codec[Box.Value] =
    discriminated[Box.Value]
      .by(byteCodec)
      .typecase(0: Byte, boxValuesEmptyCodec)
      .typecase(1: Byte, boxValuesPolyCodec)
      .typecase(2: Byte, boxValuesArbitCodec)
      .typecase(3: Byte, boxValuesAssetCodec)
      .typecase(4: Byte, boxValuesPoolRegistrationCodec)

  implicit val boxValueProtoCodec: Codec[protoModels.BoxValue] =
    discriminated[protoModels.BoxValue]
      .by(byteCodec)
      .typecase(0: Byte, boxValuesEmptyProtoCodec)
      .typecase(1: Byte, boxValuesPolyProtoCodec)
      .typecase(2: Byte, boxValuesArbitProtoCodec)
      .typecase(3: Byte, boxValuesAssetProtoCodec)
      .typecase(4: Byte, boxValuesPoolRegistrationProtoCodec)

  implicit val boxCodec: Codec[Box] =
    (Codec[TypedEvidence] :: Codec[Box.Value]).as[Box]

}

trait TetraScodecPropositionCodecs {
  self: TetraScodecPrimitiveCodecs with TetraScodecBoxCodecs =>

  implicit val propositionPermanentlyLockedCodec: Codec[Propositions.PermanentlyLocked.type] =
    emptyCodec(Propositions.PermanentlyLocked)

  implicit val propositionPermanentlyLockedProtoCodec: Codec[protoModels.PropositionPermanentlyLocked] =
    (unknownFieldSetCodec).as[protoModels.PropositionPermanentlyLocked]

  implicit val propositionsKnowledgeCurve25519Codec: Codec[Propositions.Knowledge.Curve25519] =
    Codec[VerificationKeys.Curve25519].as[Propositions.Knowledge.Curve25519]

  implicit val propositionsKnowledgeCurve25519ProtoCodec: Codec[protoModels.PropositionKnowledgeCurve25519] =
    (optionCodec[protoModels.VerificationKeyCurve25519] :: unknownFieldSetCodec)
      .as[protoModels.PropositionKnowledgeCurve25519]

  implicit val propositionsKnowledgeEd25519Codec: Codec[Propositions.Knowledge.Ed25519] =
    Codec[VerificationKeys.Ed25519].as[Propositions.Knowledge.Ed25519]

  implicit val propositionsKnowledgeEd25519ProtoCodec: Codec[protoModels.PropositionKnowledgeEd25519] =
    (optionCodec[protoModels.VerificationKeyEd25519] :: unknownFieldSetCodec)
      .as[protoModels.PropositionKnowledgeEd25519]

  implicit val propositionsKnowledgeExtendedEd25519Codec: Codec[Propositions.Knowledge.ExtendedEd25519] =
    Codec[VerificationKeys.ExtendedEd25519].as[Propositions.Knowledge.ExtendedEd25519]

  implicit val propositionsKnowledgeExtendedEd25519ProtoCodec: Codec[protoModels.PropositionKnowledgeExtendedEd25519] =
    (optionCodec[protoModels.VerificationKeyExtendedEd25519] :: unknownFieldSetCodec)
      .as[protoModels.PropositionKnowledgeExtendedEd25519]

  implicit val propositionsKnowledgeHashLockCodec: Codec[Propositions.Knowledge.HashLock] =
    Codec[Digest32].as[Propositions.Knowledge.HashLock]

  implicit val propositionsKnowledgeHashLockProtoCodec: Codec[protoModels.PropositionKnowledgeHashLock] =
    (protobufByteStringCodec :: unknownFieldSetCodec).as[protoModels.PropositionKnowledgeHashLock]

  implicit val propositionsCompositionalThresholdCodec: Codec[Propositions.Compositional.Threshold] =
    Codec
      .lazily(Codec[Int](intCodec) :: Codec[ListSet[Proposition]])
      .as[Propositions.Compositional.Threshold]

  implicit val propositionsCompositionalThresholdProtoCodec: Codec[protoModels.PropositionCompositionalThreshold] =
    Codec
      .lazily(Codec[Int](intCodec) :: Codec[Seq[protoModels.Proposition]] :: unknownFieldSetCodec)
      .as[protoModels.PropositionCompositionalThreshold]

  implicit val propositionsCompositionalAndCodec: Codec[Propositions.Compositional.And] =
    Codec
      .lazily(Codec[Proposition] :: Codec[Proposition])
      .as[Propositions.Compositional.And]

  implicit val propositionsCompositionalAndProtoCodec: Codec[protoModels.PropositionCompositionalAnd] =
    Codec
      .lazily(
        Codec[protoModels.Proposition] :: Codec[protoModels.Proposition] :: unknownFieldSetCodec
      )
      .as[protoModels.PropositionCompositionalAnd]

  implicit val propositionsCompositionalOrCodec: Codec[Propositions.Compositional.Or] =
    Codec
      .lazily(Codec[Proposition] :: Codec[Proposition])
      .as[Propositions.Compositional.Or]

  implicit val propositionsCompositionalOrProtoCodec: Codec[protoModels.PropositionCompositionalOr] =
    Codec
      .lazily(
        Codec[protoModels.Proposition] :: Codec[protoModels.Proposition] :: unknownFieldSetCodec
      )
      .as[protoModels.PropositionCompositionalOr]

  implicit val propositionsCompositionalNotCodec: Codec[Propositions.Compositional.Not] =
    Codec.lazily(Codec[Proposition]).as[Propositions.Compositional.Not]

  implicit val propositionsCompositionalNotProtoCodec: Codec[protoModels.PropositionCompositionalNot] =
    Codec
      .lazily(Codec[protoModels.Proposition] :: unknownFieldSetCodec)
      .as[protoModels.PropositionCompositionalNot]

  implicit val propositionsContextualHeightLockCodec: Codec[Propositions.Contextual.HeightLock] =
    uLongCodec.as[Propositions.Contextual.HeightLock]

  implicit val propositionsContextualHeightLockProtoCodec: Codec[protoModels.PropositionContextualHeightLock] =
    (uLongCodec :: unknownFieldSetCodec).as[protoModels.PropositionContextualHeightLock]

  implicit val boxLocationCodec: Codec[BoxLocation] =
    discriminated[BoxLocation]
      .by(byteCodec)
      .typecase(0: Byte, shortCodec.as[BoxLocations.Input])
      .typecase(1: Byte, shortCodec.as[BoxLocations.Output])

  implicit val boxLocationIOInputProtoCodec: Codec[protoModels.BoxLocation.IO.INPUT.type] =
    emptyCodec(protoModels.BoxLocation.IO.INPUT)

  implicit val boxLocationIOOutputProtoCodec: Codec[protoModels.BoxLocation.IO.OUTPUT.type] =
    emptyCodec(protoModels.BoxLocation.IO.OUTPUT)

  implicit val boxLocationIOProtoCodec: Codec[protoModels.BoxLocation.IO] =
    discriminated[protoModels.BoxLocation.IO]
      .by(byteCodec)
      .typecase(0: Byte, boxLocationIOInputProtoCodec)
      .typecase(1: Byte, boxLocationIOOutputProtoCodec)

  implicit val boxLocationProtoCodec: Codec[protoModels.BoxLocation] =
    (intCodec :: boxLocationIOProtoCodec :: unknownFieldSetCodec).as[protoModels.BoxLocation]

  implicit val propositionsContextualRequiredTransactionIORequirementCodec
    : Codec[Propositions.Contextual.RequiredTransactionIO.Requirement] =
    Codec.lazily(
      (Codec[Box] :: Codec[BoxLocation]).as[Propositions.Contextual.RequiredTransactionIO.Requirement]
    )

  implicit val propositionsContextualRequiredTransactionIORequirementProtoCodec
    : Codec[protoModels.PropositionContextualRequiredTransactionIO.Requirement] =
    Codec.lazily(
      (optionCodec[protoModels.Box] :: optionCodec[protoModels.BoxLocation] :: unknownFieldSetCodec)
        .as[protoModels.PropositionContextualRequiredTransactionIO.Requirement]
    )

  implicit val propositionsContextualRequiredTransactionIOCodec: Codec[Propositions.Contextual.RequiredTransactionIO] =
    Codec.lazily(
      Codec[NonEmptyChain[Propositions.Contextual.RequiredTransactionIO.Requirement]]
        .as[Propositions.Contextual.RequiredTransactionIO]
    )

  implicit val propositionsContextualRequiredTransactionIOProtoCodec
    : Codec[protoModels.PropositionContextualRequiredTransactionIO] =
    Codec.lazily(
      (Codec[Seq[protoModels.PropositionContextualRequiredTransactionIO.Requirement]] :: unknownFieldSetCodec)
        .as[protoModels.PropositionContextualRequiredTransactionIO]
    )

  implicit val propositionCodec: Codec[Proposition] =
    discriminated[Proposition]
      .by(byteCodec)
      .typecase[Propositions.PermanentlyLocked.type](0: Byte, propositionPermanentlyLockedCodec)
      .typecase[Propositions.Knowledge.Curve25519](1: Byte, propositionsKnowledgeCurve25519Codec)
      .typecase[Propositions.Knowledge.Ed25519](2: Byte, propositionsKnowledgeEd25519Codec)
      .typecase[Propositions.Knowledge.ExtendedEd25519](3: Byte, propositionsKnowledgeExtendedEd25519Codec)
      .typecase[Propositions.Knowledge.HashLock](4: Byte, propositionsKnowledgeHashLockCodec)
      .typecase[Propositions.Compositional.Threshold](5: Byte, propositionsCompositionalThresholdCodec)
      .typecase[Propositions.Compositional.And](6: Byte, propositionsCompositionalAndCodec)
      .typecase[Propositions.Compositional.Or](7: Byte, propositionsCompositionalOrCodec)
      .typecase[Propositions.Compositional.Not](8: Byte, propositionsCompositionalNotCodec)
      .typecase[Propositions.Contextual.HeightLock](9: Byte, propositionsContextualHeightLockCodec)
      .typecase[Propositions.Contextual.RequiredTransactionIO](
        10: Byte,
        propositionsContextualRequiredTransactionIOCodec
      )

  implicit val propositionProtoCodec: Codec[protoModels.Proposition] =
    discriminated[protoModels.Proposition]
      .by(byteCodec)
      .typecase[protoModels.PropositionPermanentlyLocked](0: Byte, propositionPermanentlyLockedProtoCodec)
      .typecase[protoModels.PropositionKnowledgeCurve25519](1: Byte, propositionsKnowledgeCurve25519ProtoCodec)
      .typecase[protoModels.PropositionKnowledgeEd25519](2: Byte, propositionsKnowledgeEd25519ProtoCodec)
      .typecase[protoModels.PropositionKnowledgeExtendedEd25519](
        3: Byte,
        propositionsKnowledgeExtendedEd25519ProtoCodec
      )
      .typecase[protoModels.PropositionKnowledgeHashLock](4: Byte, propositionsKnowledgeHashLockProtoCodec)
      .typecase[protoModels.PropositionCompositionalThreshold](
        5: Byte,
        propositionsCompositionalThresholdProtoCodec
      )
      .typecase[protoModels.PropositionCompositionalAnd](6: Byte, propositionsCompositionalAndProtoCodec)
      .typecase[protoModels.PropositionCompositionalOr](7: Byte, propositionsCompositionalOrProtoCodec)
      .typecase[protoModels.PropositionCompositionalNot](8: Byte, propositionsCompositionalNotProtoCodec)
      .typecase[protoModels.PropositionContextualHeightLock](
        9: Byte,
        propositionsContextualHeightLockProtoCodec
      )
      .typecase[protoModels.PropositionContextualRequiredTransactionIO](
        10: Byte,
        propositionsContextualRequiredTransactionIOProtoCodec
      )

}

trait TetraScodecProofCodecs {
  self: TetraScodecPrimitiveCodecs with TetraScodecVerificationKeyCodecs =>

  // TODO Remove after full model replacement
  implicit val proofsUndefinedCodec: Codec[Proofs.Undefined.type] =
    emptyCodec(Proofs.Undefined)

  implicit val proofsUndefinedProtoCodec: Codec[protoModels.ProofUndefined] =
    (unknownFieldSetCodec).as[protoModels.ProofUndefined]

  // TODO Remove after full model replacement
  implicit val proofSignatureCurve25519: Codec[Proofs.Knowledge.Curve25519] =
    strictSizedBytesCodec[Proofs.Knowledge.Curve25519.Length].as[Proofs.Knowledge.Curve25519]

  implicit val proofSignatureCurve25519ProtoCodec: Codec[protoModels.ProofKnowledgeCurve25519] =
    (protobufByteStringCodec :: unknownFieldSetCodec).as[protoModels.ProofKnowledgeCurve25519]

  // TODO Remove after full model replacement
  implicit val proofSignatureEd25519Codec: Codec[Proofs.Knowledge.Ed25519] =
    strictSizedBytesCodec[Proofs.Knowledge.Ed25519.Length].as[Proofs.Knowledge.Ed25519]

  implicit val proofSignatureEd25519ProtoCodec: Codec[protoModels.ProofKnowledgeEd25519] =
    (protobufByteStringCodec :: unknownFieldSetCodec).as[protoModels.ProofKnowledgeEd25519]

  implicit val consensusProofSignatureEd25519Codec: Codec[cryptoModels.SignatureEd25519] =
    (protobufByteStringCodec :: unknownFieldSetCodec).as[cryptoModels.SignatureEd25519]

  // TODO Remove after full model replacement
  implicit val proofSignatureVrfCodec: Codec[Proofs.Knowledge.VrfEd25519] =
    strictSizedBytesCodec[Proofs.Knowledge.VrfEd25519.Length].as[Proofs.Knowledge.VrfEd25519]

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
      optionCodec(criptoVkEd25519Codec) ::
        optionCodec(consensusProofSignatureEd25519Codec) ::
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
      optionCodec(consensusProofSignatureKesSumCodec) ::
        optionCodec(consensusProofSignatureKesSumCodec) ::
        protobufByteStringCodec ::
        unknownFieldSetCodec
    ).as[consensusModels.SignatureKesProduct]

  // TODO Remove after full model replacement
  implicit val proofsKnowledgeHashLockCodec: Codec[Proofs.Knowledge.HashLock] =
    byteVectorCodec.as[Proofs.Knowledge.HashLock]

  implicit val proofsKnowledgeHashLockProtoCodec: Codec[protoModels.ProofKnowledgeHashLock] =
    (protobufByteStringCodec :: unknownFieldSetCodec).as[protoModels.ProofKnowledgeHashLock]

  // TODO Remove after full model replacement
  implicit val proofsCompositionalThresholdCodec: Codec[Proofs.Compositional.Threshold] =
    Codec
      .lazily(Codec[List[Proof]])
      .as[Proofs.Compositional.Threshold]

  implicit val proofsCompositionalThresholdProtoCodec: Codec[protoModels.ProofCompositionalThreshold] =
    Codec
      .lazily(Codec[Seq[protoModels.Proof]] :: unknownFieldSetCodec)
      .as[protoModels.ProofCompositionalThreshold]

  // TODO Remove after full model replacement
  implicit val proofsCompositionalAndCodec: Codec[Proofs.Compositional.And] =
    Codec
      .lazily(Codec[Proof] :: Codec[Proof])
      .as[Proofs.Compositional.And]

  implicit val proofsCompositionalAndProtoCodec: Codec[protoModels.ProofCompositionalAnd] =
    Codec
      .lazily(Codec[protoModels.Proof] :: Codec[protoModels.Proof] :: unknownFieldSetCodec)
      .as[protoModels.ProofCompositionalAnd]

  // TODO Remove after full model replacement
  implicit val proofsCompositionalOrCodec: Codec[Proofs.Compositional.Or] =
    Codec
      .lazily(Codec[Proof] :: Codec[Proof])
      .as[Proofs.Compositional.Or]

  implicit val proofsCompositionalOrProtoCodec: Codec[protoModels.ProofCompositionalOr] =
    Codec
      .lazily(Codec[protoModels.Proof] :: Codec[protoModels.Proof] :: unknownFieldSetCodec)
      .as[protoModels.ProofCompositionalOr]

  // TODO Remove after full model replacement
  implicit val proofsCompositionalNotCodec: Codec[Proofs.Compositional.Not] =
    Codec
      .lazily(Codec[Proof])
      .as[Proofs.Compositional.Not]

  implicit val proofsCompositionalNotProtoCodec: Codec[protoModels.ProofCompositionalNot] =
    Codec
      .lazily(Codec[protoModels.Proof] :: unknownFieldSetCodec)
      .as[protoModels.ProofCompositionalNot]

  // TODO Remove after full model replacement
  implicit val proofsContextualHeightLockCodec: Codec[Proofs.Contextual.HeightLock] =
    emptyCodec(Proofs.Contextual.HeightLock())

  implicit val proofsContextualHeightLockProtoCodec: Codec[protoModels.ProofContextualHeightLock] =
    (unknownFieldSetCodec).as[protoModels.ProofContextualHeightLock]

  // TODO Remove after full model replacement
  implicit val proofsContextualRequiredTransactionIOCodec: Codec[Proofs.Contextual.RequiredTransactionIO] =
    emptyCodec(Proofs.Contextual.RequiredTransactionIO())

  implicit val proofsContextualRequiredTransactionIOProtoCodec
    : Codec[protoModels.ProofContextualRequiredTransactionIO] =
    (unknownFieldSetCodec).as[protoModels.ProofContextualRequiredTransactionIO]

  // TODO Remove after full model replacement
  implicit val proofCodec: Codec[Proof] =
    discriminated[Proof]
      .by(byteCodec)
      .typecase(0: Byte, proofsUndefinedCodec)
      .typecase(1: Byte, proofSignatureCurve25519)
      .typecase(2: Byte, proofSignatureEd25519Codec)
      .typecase(3: Byte, proofSignatureVrfCodec)
      .typecase(4: Byte, proofSignatureKesSumCodec)
      .typecase(5: Byte, proofSignatureKesProductCodec)
      .typecase(6: Byte, proofsKnowledgeHashLockCodec)
      .typecase(7: Byte, proofsCompositionalThresholdCodec)
      .typecase(8: Byte, proofsCompositionalAndCodec)
      .typecase(9: Byte, proofsCompositionalOrCodec)
      .typecase(10: Byte, proofsCompositionalNotCodec)
      .typecase(11: Byte, proofsContextualHeightLockCodec)
      .typecase(12: Byte, proofsContextualRequiredTransactionIOCodec)

  implicit val proofProtoCodec: Codec[protoModels.Proof] =
    discriminated[protoModels.Proof]
      .by(byteCodec)
      .typecase(0: Byte, proofsUndefinedProtoCodec)
      .typecase(1: Byte, proofSignatureCurve25519ProtoCodec)
      .typecase(2: Byte, proofSignatureEd25519ProtoCodec)
      .typecase(3: Byte, proofSignatureVrfProtoCodec)
      .typecase(4: Byte, proofSignatureKesSumProtoCodec)
      .typecase(5: Byte, proofSignatureKesProductProtoCodec)
      .typecase(6: Byte, proofsKnowledgeHashLockProtoCodec)
      .typecase(7: Byte, proofsCompositionalThresholdProtoCodec)
      .typecase(8: Byte, proofsCompositionalAndProtoCodec)
      .typecase(9: Byte, proofsCompositionalOrProtoCodec)
      .typecase(10: Byte, proofsCompositionalNotProtoCodec)
      .typecase(11: Byte, proofsContextualHeightLockProtoCodec)
      .typecase(12: Byte, proofsContextualRequiredTransactionIOProtoCodec)
}

trait TetraScodecTransactionCodecs {
  self: TetraScodecPrimitiveCodecs
    with TetraScodecPropositionCodecs
    with TetraScodecAddressCodecs
    with TetraScodecProofCodecs
    with TetraScodecBoxCodecs =>

  // TODO Remove after full model replacement
  implicit val transactionInputCodec: Codec[Transaction.Input] =
    (Codec[Box.Id] :: Codec[Proposition] :: Codec[Proof] :: Codec[Box.Value]).as[Transaction.Input]

  // TODO Remove after full model replacement
  implicit val coinOutputCodec: Codec[Transaction.Output] =
    (Codec[FullAddress] :: Codec[Box.Value] :: Codec[Boolean])
      .as[Transaction.Output]

  implicit val coinOutputProtoCodec: Codec[protoModels.Transaction.UnspentOutput] =
    (optionCodec[protoModels.FullAddress] ::
      Codec[protoModels.BoxValue] ::
      Codec[Boolean] ::
      Codec[ByteString] ::
      unknownFieldSetCodec)
      .as[protoModels.Transaction.UnspentOutput]

  // TODO Remove after full model replacement
  implicit val transactionScheduleCodec: Codec[Transaction.Schedule] =
    (Codec[Timestamp](uLongCodec) :: Codec[Slot](uLongCodec) :: Codec[Slot](uLongCodec))
      .as[Transaction.Schedule]

  implicit val transactionScheduleProtoCodec: Codec[protoModels.Transaction.Schedule] =
    (Codec[Timestamp](uLongCodec) :: Codec[Slot](uLongCodec) :: Codec[Slot](uLongCodec) :: unknownFieldSetCodec)
      .as[protoModels.Transaction.Schedule]

  // TODO Remove after full model replacement
  implicit val transactionCodec: Codec[Transaction] =
    (
      Codec[Chain[Transaction.Input]] ::
        Codec[Chain[Transaction.Output]] ::
        Codec[Transaction.Schedule] ::
        Codec[Option[Transaction.DataTetra]]
    ).as[Transaction]

  implicit val transactionInputProtoCodec: Codec[protoModels.Transaction.Input] =
    (
      optionCodec[protoModels.Box.Id] ::
        Codec[protoModels.Proposition] ::
        Codec[protoModels.Proof] ::
        Codec[protoModels.BoxValue] ::
        unknownFieldSetCodec
    ).as[protoModels.Transaction.Input]

  implicit val transactionProtoCodec: Codec[protoModels.Transaction] =
    (
      Codec[Seq[protoModels.Transaction.Input]] ::
        Codec[Seq[protoModels.Transaction.UnspentOutput]] ::
        optionCodec[protoModels.Transaction.Schedule] ::
        protobufByteStringCodec ::
        unknownFieldSetCodec
    ).as[protoModels.Transaction]

  // TODO Remove after full model replacement
  implicit val unprovenTransactionCodec: Codec[Transaction.Unproven] =
    (
      Codec[Chain[Transaction.Unproven.Input]] ::
        Codec[Chain[Transaction.Output]] ::
        Codec[Transaction.Schedule] ::
        Codec[Option[Transaction.DataTetra]]
    ).as[Transaction.Unproven]

  implicit val unprovenInputTransactionProtoCodec: Codec[Transaction.Unproven.InputProto] =
    (
      optionCodec[protoModels.Box.Id] ::
        Codec[protoModels.Proposition] ::
        Codec[protoModels.BoxValue]
    ).as[Transaction.Unproven.InputProto]

  implicit val unprovenTransactionProtoCodec: Codec[Transaction.UnprovenProto] =
    (
      Codec[Chain[Transaction.Unproven.InputProto]] ::
        Codec[Chain[protoModels.Transaction.UnspentOutput]] ::
        Codec[Option[protoModels.Transaction.Schedule]] ::
        protobufByteStringCodec
    ).as[Transaction.UnprovenProto]
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

  implicit val consensusOperationalCertificateCodec: Codec[consensusModels.OperationalCertificate] =
    (optionCodec(consensusVkKesProductCodec) ::
      optionCodec(consensusProofSignatureKesProductCodec) ::
      optionCodec(criptoVkEd25519Codec) ::
      optionCodec(consensusProofSignatureEd25519Codec) ::
      unknownFieldSetCodec).as[consensusModels.OperationalCertificate]

  implicit val partialOperationalCertificateCodec
    : Codec[legacyModels.BlockHeader.Unsigned.PartialOperationalCertificate] =
    (vkKesProductCodec :: proofSignatureKesProductCodec :: vkEd25519Codec)
      .as[legacyModels.BlockHeader.Unsigned.PartialOperationalCertificate]

  implicit val partialOperationalCertificateConsensusCodec
    : Codec[legacyModels.BlockHeader.UnsignedConsensus.PartialOperationalCertificate] =
    (optionCodec(consensusVkKesProductCodec) ::
      optionCodec(consensusProofSignatureKesProductCodec) ::
      optionCodec(criptoVkEd25519Codec))
      .as[legacyModels.BlockHeader.UnsignedConsensus.PartialOperationalCertificate]

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

  implicit val consensusBlockIdCodec: Codec[consensusModels.BlockId] =
    (
      protobufByteStringCodec ::
        unknownFieldSetCodec
    ).as[consensusModels.BlockId]

  implicit val consensusBlockHeaderCodec: Codec[consensusModels.BlockHeader] = (
    optionCodec(consensusBlockIdCodec) :: // parentHeaderId
      longCodec :: // parentSlot
      protobufByteStringCodec :: // txRoot
      protobufByteStringCodec :: // bloomFilter
      longCodec :: // timestamp
      longCodec :: // height
      longCodec :: // slot
      optionCodec(consensusEligibilityCertificateCodec) ::
      optionCodec(consensusOperationalCertificateCodec) ::
      protobufByteStringCodec :: // metadata
      protobufByteStringCodec :: // address
      unknownFieldSetCodec
  ).as[consensusModels.BlockHeader]

  implicit val slotIdCodec: Codec[SlotId] =
    (Codec[Slot](uLongCodec) :: Codec[TypedIdentifier]).as[SlotId]

  implicit val slotDataCodec: Codec[SlotData] =
    (Codec[SlotId] :: Codec[SlotId] :: Codec[Rho] :: Codec[Eta] :: Codec[Long](uLongCodec)).as[SlotData]

  implicit val unsignedBlockHeaderCodec: Codec[legacyModels.BlockHeader.Unsigned] =
    (
      typedBytesCodec ::
        longCodec ::
        Codec[TxRoot] ::
        Codec[BloomFilter] ::
        longCodec ::
        longCodec ::
        longCodec ::
        eligibilityCertificateCodec ::
        partialOperationalCertificateCodec ::
        optionCodec(maxSizedCodec[Latin1Data, Lengths.`32`.type]) ::
        stakingAddressesOperatorCodec
    ).as[legacyModels.BlockHeader.Unsigned]

  implicit val unsignedConsensusBlockHeaderCodec: Codec[legacyModels.BlockHeader.UnsignedConsensus] =
    (
      optionCodec(consensusBlockIdCodec) ::
        longCodec ::
        protobufByteStringCodec ::
        protobufByteStringCodec ::
        longCodec ::
        longCodec ::
        longCodec ::
        optionCodec(consensusEligibilityCertificateCodec) ::
        partialOperationalCertificateConsensusCodec ::
        protobufByteStringCodec ::
        protobufByteStringCodec
    ).as[legacyModels.BlockHeader.UnsignedConsensus]

  implicit val blockBodyCodec: Codec[BlockBody] = listSetCodec[TypedIdentifier]

}
