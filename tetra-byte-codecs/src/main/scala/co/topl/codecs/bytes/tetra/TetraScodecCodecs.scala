package co.topl.codecs.bytes.tetra

import cats.data.Chain
import cats.implicits._
import co.topl.codecs.bytes.scodecs._
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility._
import scodec.codecs.{discriminated, lazily}
import scodec.{Attempt, Codec, Err}
import shapeless.{::, HList, HNil}

import scala.collection.immutable.ListSet

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

  implicit val networkPrefixCodec: Codec[NetworkPrefix] =
    Codec[Byte].xmap(NetworkPrefix(_), _.value)

  implicit val typedEvidenceCodec: Codec[TypedEvidence] =
    (Codec[TypePrefix] :: Codec[Evidence]).as[TypedEvidence]

  implicit val spendingAddressCodec: Codec[SpendingAddress] =
    Codec[TypedEvidence].as[SpendingAddress] // TODO: Checksum

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

  implicit val vkCurve25519Codec: Codec[VerificationKeys.Curve25519] =
    strictSizedBytesCodec[VerificationKeys.Curve25519.Length]
      .as[VerificationKeys.Curve25519]

  implicit val vkEd25519Codec: Codec[VerificationKeys.Ed25519] =
    strictSizedBytesCodec[VerificationKeys.Ed25519.Length]
      .as[VerificationKeys.Ed25519]

  implicit val vkExtendedEd25519Codec: Codec[VerificationKeys.ExtendedEd25519] =
    (vkEd25519Codec :: strictSizedBytesCodec[SecretKeys.ExtendedEd25519.ChainCodeLength])
      .as[VerificationKeys.ExtendedEd25519]

  implicit val vkVrfCodec: Codec[VerificationKeys.VrfEd25519] =
    strictSizedBytesCodec[VerificationKeys.VrfEd25519.Length]
      .as[VerificationKeys.VrfEd25519]

  implicit val vkKesSumCodec: Codec[VerificationKeys.KesSum] =
    (strictSizedBytesCodec[VerificationKeys.KesSum.Length] :: intCodec)
      .as[VerificationKeys.KesSum]

  implicit val vkKesProductCodec: Codec[VerificationKeys.KesProduct] =
    (strictSizedBytesCodec[VerificationKeys.KesProduct.Length] :: intCodec)
      .as[VerificationKeys.KesProduct]
}

trait TetraScodecAddressCodecs {
  self: TetraScodecPrimitiveCodecs with TetraScodecVerificationKeyCodecs =>

  implicit val stakingAddressesPoolCodec: Codec[StakingAddresses.Pool] =
    vkEd25519Codec.as[StakingAddresses.Pool]

  implicit val stakingAddressCodec: Codec[StakingAddress] =
    discriminated[StakingAddress]
      .by(byteCodec)
      .typecase(0: Byte, stakingAddressesPoolCodec)

  implicit val fullAddressCodec: Codec[FullAddress] =
    (Codec[NetworkPrefix] :: Codec[SpendingAddress] :: Codec[StakingAddress] :: Codec[Proofs.Knowledge.Ed25519])
      .as[FullAddress]

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

  implicit val boxValuesEmptyCodec: Codec[Box.Values.Empty.type] =
    emptyCodec(Box.Values.Empty)

  implicit val boxValuesPolyCodec: Codec[Box.Values.Poly] =
    int128Codec.as[Box.Values.Poly]

  implicit val boxValuesArbitCodec: Codec[Box.Values.Arbit] =
    int128Codec.as[Box.Values.Arbit]

  implicit val boxValuesAssetCodec: Codec[Box.Values.Asset] =
    (Codec[Int128] :: Codec[Box.Values.Asset.Code] :: Codec[Bytes] :: Codec[Option[
      Sized.Max[Latin1Data, Lengths.`127`.type]
    ]]).as[Box.Values.Asset]

  implicit val boxValuesPoolRegistrationCodec: Codec[Box.Values.Registrations.Pool] =
    Codec[Proofs.Knowledge.KesProduct].as[Box.Values.Registrations.Pool]

  implicit val boxValueCode: Codec[Box.Value] =
    discriminated[Box.Value]
      .by(byteCodec)
      .typecase(0: Byte, boxValuesEmptyCodec)
      .typecase(1: Byte, boxValuesPolyCodec)
      .typecase(2: Byte, boxValuesArbitCodec)
      .typecase(3: Byte, boxValuesAssetCodec)
      .typecase(4: Byte, boxValuesPoolRegistrationCodec)

  implicit val boxCodec: Codec[Box] =
    (Codec[TypedEvidence] :: Codec[Box.Value]).as[Box]

}

trait TetraScodecPropositionCodecs {
  self: TetraScodecPrimitiveCodecs with TetraScodecBoxCodecs =>

  implicit val propositionPermanentlyLockedCodec: Codec[Propositions.PermanentlyLocked.type] =
    emptyCodec(Propositions.PermanentlyLocked)

  implicit val propositionsKnowledgeCurve25519Codec: Codec[Propositions.Knowledge.Curve25519] =
    Codec[VerificationKeys.Curve25519].as[Propositions.Knowledge.Curve25519]

  implicit val propositionsKnowledgeEd25519Codec: Codec[Propositions.Knowledge.Ed25519] =
    Codec[VerificationKeys.Ed25519].as[Propositions.Knowledge.Ed25519]

  implicit val propositionsKnowledgeExtendedEd25519Codec: Codec[Propositions.Knowledge.ExtendedEd25519] =
    Codec[VerificationKeys.ExtendedEd25519].as[Propositions.Knowledge.ExtendedEd25519]

  implicit val propositionsKnowledgeHashLockCodec: Codec[Propositions.Knowledge.HashLock] =
    Codec[Digest32].as[Propositions.Knowledge.HashLock]

  implicit val propositionsCompositionalThresholdCodec: Codec[Propositions.Compositional.Threshold] =
    Codec
      .lazily((Codec[Int](intCodec) :: Codec[ListSet[Proposition]]))
      .as[Propositions.Compositional.Threshold]

  implicit val propositionsCompositionalAndCodec: Codec[Propositions.Compositional.And] =
    Codec
      .lazily((Codec[Proposition] :: Codec[Proposition]))
      .as[Propositions.Compositional.And]

  implicit val propositionsCompositionalOrCodec: Codec[Propositions.Compositional.Or] =
    Codec
      .lazily((Codec[Proposition] :: Codec[Proposition]))
      .as[Propositions.Compositional.Or]

  implicit val propositionsCompositionalNotCodec: Codec[Propositions.Compositional.Not] =
    Codec.lazily(Codec[Proposition]).as[Propositions.Compositional.Not]

  implicit val propositionsContextualHeightLockCodec: Codec[Propositions.Contextual.HeightLock] =
    uLongCodec.as[Propositions.Contextual.HeightLock]

  implicit val boxLocationCodec: Codec[BoxLocation] =
    discriminated[BoxLocation]
      .by(byteCodec)
      .typecase(0: Byte, emptyCodec(BoxLocations.Input))
      .typecase(1: Byte, emptyCodec(BoxLocations.Output))

  implicit val propositionsContextualRequiredBoxStateCodec: Codec[Propositions.Contextual.RequiredBoxState] =
    Codec.lazily(
      (Codec[BoxLocation] :: Codec[List[(Int, Box)]](listCodec(tupleCodec(intCodec, boxCodec))))
        .as[Propositions.Contextual.RequiredBoxState]
    )

  implicit val propositionsScriptJsCodec: Codec[Propositions.Script.JS] =
    intStringCodec.xmap(bs => Propositions.Script.JS(Propositions.Script.JS.JSScript(bs)), _.script.value)

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
      .typecase[Propositions.Contextual.RequiredBoxState](10: Byte, propositionsContextualRequiredBoxStateCodec)
      .typecase[Propositions.Script.JS](11: Byte, propositionsScriptJsCodec)

}

trait TetraScodecProofCodecs {
  self: TetraScodecPrimitiveCodecs with TetraScodecVerificationKeyCodecs =>

  implicit val proofsFalseCodec: Codec[Proofs.False.type] =
    emptyCodec(Proofs.False)

  implicit val proofSignatureCurve25519: Codec[Proofs.Knowledge.Curve25519] =
    strictSizedBytesCodec[Proofs.Knowledge.Curve25519.Length].as[Proofs.Knowledge.Curve25519]

  implicit val proofSignatureEd25519Codec: Codec[Proofs.Knowledge.Ed25519] =
    strictSizedBytesCodec[Proofs.Knowledge.Ed25519.Length].as[Proofs.Knowledge.Ed25519]

  implicit val proofSignatureVrfCodec: Codec[Proofs.Knowledge.VrfEd25519] =
    strictSizedBytesCodec[Proofs.Knowledge.VrfEd25519.Length].as[Proofs.Knowledge.VrfEd25519]

  implicit val proofSignatureKesSumCodec: Codec[Proofs.Knowledge.KesSum] =
    (vkEd25519Codec :: proofSignatureEd25519Codec :: vectorCodec[
      Sized.Strict[Bytes, Proofs.Knowledge.KesSum.DigestLength]
    ]).as[Proofs.Knowledge.KesSum]

  implicit val proofSignatureKesProductCodec: Codec[Proofs.Knowledge.KesProduct] =
    (proofSignatureKesSumCodec :: proofSignatureKesSumCodec :: strictSizedBytesCodec[
      Proofs.Knowledge.KesProduct.DigestLength
    ]).as[Proofs.Knowledge.KesProduct]

  implicit val proofsKnowledgeHashLockCodec: Codec[Proofs.Knowledge.HashLock] =
    (Codec[Digest32] :: byteCodec)
      .as[Proofs.Knowledge.HashLock]

  implicit val proofsCompositionalThresholdCodec: Codec[Proofs.Compositional.Threshold] =
    Codec
      .lazily(Codec[List[Proof]])
      .as[Proofs.Compositional.Threshold]

  implicit val proofsCompositionalAndCodec: Codec[Proofs.Compositional.And] =
    Codec
      .lazily(Codec[Proof] :: Codec[Proof])
      .as[Proofs.Compositional.And]

  implicit val proofsCompositionalOrCodec: Codec[Proofs.Compositional.Or] =
    Codec
      .lazily(Codec[Proof] :: Codec[Proof])
      .as[Proofs.Compositional.Or]

  implicit val proofsCompositionalNotCodec: Codec[Proofs.Compositional.Not] =
    Codec
      .lazily(Codec[Proof])
      .as[Proofs.Compositional.Not]

  implicit val proofsContextualHeightLockCodec: Codec[Proofs.Contextual.HeightLock] =
    emptyCodec(Proofs.Contextual.HeightLock())

  implicit val proofsContextualRequiredBoxStateCodec: Codec[Proofs.Contextual.RequiredBoxState] =
    emptyCodec(Proofs.Contextual.RequiredBoxState())

  implicit val proofsScriptJsCodec: Codec[Proofs.Script.JS] =
    intStringCodec.as[Proofs.Script.JS]

  implicit val proofCodec: Codec[Proof] =
    discriminated[Proof]
      .by(byteCodec)
      .typecase(0: Byte, proofsFalseCodec)
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
      .typecase(12: Byte, proofsContextualRequiredBoxStateCodec)
      .typecase(13: Byte, proofsScriptJsCodec)
}

trait TetraScodecTransactionCodecs {
  self: TetraScodecPrimitiveCodecs
    with TetraScodecPropositionCodecs
    with TetraScodecAddressCodecs
    with TetraScodecProofCodecs
    with TetraScodecBoxCodecs =>

  implicit val coinOutputCodec: Codec[Transaction.Output] =
    (Codec[FullAddress] :: Codec[Box.Value] :: Codec[Boolean])
      .as[Transaction.Output]

  implicit val transactionCodec: Codec[Transaction] =
    (
      Codec[Chain[Transaction.Input]] ::
        Codec[Chain[Transaction.Output]] ::
        Codec[Timestamp](uLongCodec) ::
        Codec[Option[TransactionData]]
    ).as[Transaction]

  implicit val unprovenTransactionCodec: Codec[Transaction.Unproven] =
    (
      Codec[Chain[Transaction.Unproven.Input]] ::
        Codec[Chain[Transaction.Output]] ::
        Codec[Timestamp](uLongCodec) ::
        Codec[Option[TransactionData]]
    ).as[Transaction.Unproven]
}

trait TetraScodecBlockCodecs {
  self: TetraScodecPrimitiveCodecs
    with TetraScodecVerificationKeyCodecs
    with TetraScodecProofCodecs
    with TetraScodecAddressCodecs =>

  implicit val eligibilityCertificateCodec: Codec[EligibilityCertificate] =
    (proofSignatureVrfCodec :: vkVrfCodec :: Codec[Evidence] :: Codec[Eta])
      .as[EligibilityCertificate]

  implicit val operationalCertificateCodec: Codec[OperationalCertificate] =
    (vkKesProductCodec :: proofSignatureKesProductCodec :: vkEd25519Codec :: proofSignatureEd25519Codec)
      .as[OperationalCertificate]

  implicit val partialOperationalCertificateCodec: Codec[BlockHeaderV2.Unsigned.PartialOperationalCertificate] =
    (vkKesProductCodec :: proofSignatureKesProductCodec :: vkEd25519Codec)
      .as[BlockHeaderV2.Unsigned.PartialOperationalCertificate]

  implicit val blockHeaderV2Codec: Codec[BlockHeaderV2] =
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
        stakingAddressesPoolCodec
    ).as[BlockHeaderV2]

  implicit val slotIdCodec: Codec[SlotId] =
    (Codec[Slot](uLongCodec) :: Codec[TypedIdentifier]).as[SlotId]

  implicit val slotDataCodec: Codec[SlotData] =
    (Codec[SlotId] :: Codec[SlotId] :: Codec[Rho] :: Codec[Eta] :: Codec[Long](uLongCodec)).as[SlotData]

  implicit val unsignedBlockHeaderV2Codec: Codec[BlockHeaderV2.Unsigned] =
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
        stakingAddressesPoolCodec
    ).as[BlockHeaderV2.Unsigned]

  implicit val blockBodyV2Codec: Codec[BlockBodyV2] = listCodec[TypedIdentifier]

}
