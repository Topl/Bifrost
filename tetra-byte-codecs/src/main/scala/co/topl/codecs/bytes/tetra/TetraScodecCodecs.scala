package co.topl.codecs.bytes.tetra

import cats.data.NonEmptyChain
import cats.implicits._
import co.topl.codecs.bytes.scodecs._
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility._
import scodec.bits.BitVector
import scodec.codecs.{bytes, discriminated, lazily}
import scodec.{Attempt, Codec, DecodeResult, Err}
import shapeless.{::, HList, HNil}

import scala.collection.immutable.{ListMap, ListSet}
import scala.reflect.ClassTag

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

  implicit val byteVectorCodec: Codec[Bytes] =
    arrayCodec[Byte].xmap(arr => Bytes(arr), _.toArray)

  implicit val typedBytesCodec: Codec[TypedBytes] =
    (byteCodec :: byteVectorCodec)
      .xmapc { case prefix :: data :: _ =>
        TypedBytes(prefix, data)
      }(t => HList(t.typePrefix, t.dataBytes))
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

  implicit val networkPrefixCodec: Codec[NetworkPrefix] =
    Codec[Byte].xmapc(b => NetworkPrefix(b))(_.value)

  implicit val typedEvidenceCodec: Codec[TypedEvidence] =
    (Codec[TypePrefix] :: Codec[Evidence])
      .xmapc { case prefix :: evidence :: HNil => TypedEvidence(prefix, evidence) }(t =>
        HList(t.typePrefix, t.evidence)
      )

  implicit val dionAddressCodec: Codec[DionAddress] =
    (Codec[NetworkPrefix] :: Codec[TypedEvidence]) // TODO: Checksum
      .xmapc { case prefix :: typedEvidence :: HNil => DionAddress(prefix, typedEvidence) }(t =>
        HList(t.networkPrefix, t.typedEvidence)
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

  implicit val proofsFalseCodec: Codec[Proofs.False.type] =
    emptyCodec(Proofs.False)

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

  implicit val proofsKnowledgeHashLockCodec: Codec[Proofs.Knowledge.HashLock] =
    (Codec[Digest32] :: byteCodec)
      .xmapc { case salt :: value :: HNil => Proofs.Knowledge.HashLock(salt, value) }(t => HList(t.salt, t.value))

  implicit val proofsCompositionalThresholdCodec: Codec[Proofs.Compositional.Threshold] =
    Codec
      .lazily(Codec[List[Proof]])
      .xmap(proofs => Proofs.Compositional.Threshold(proofs), _.proofs)

  implicit val proofsCompositionalAndCodec: Codec[Proofs.Compositional.And] =
    Codec
      .lazily(Codec[Proof] :: Codec[Proof])
      .xmapc { case a :: b :: HNil => Proofs.Compositional.And(a, b) }(t => HList(t.a, t.b))

  implicit val proofsCompositionalOrCodec: Codec[Proofs.Compositional.Or] =
    Codec
      .lazily(Codec[Proof] :: Codec[Proof])
      .xmapc { case a :: b :: HNil => Proofs.Compositional.Or(a, b) }(t => HList(t.a, t.b))

  implicit val proofsCompositionalNotCodec: Codec[Proofs.Compositional.Not] =
    Codec.lazily(Codec[Proof]).xmap(Proofs.Compositional.Not, _.a)

  implicit val proofsContextualHeightLockCodec: Codec[Proofs.Contextual.HeightLock] =
    emptyCodec(Proofs.Contextual.HeightLock())

  implicit val proofsContextualRequiredBoxStateCodec: Codec[Proofs.Contextual.RequiredBoxState] =
    emptyCodec(Proofs.Contextual.RequiredBoxState())

  implicit val proofsScriptJsCodec: Codec[Proofs.Script.JS] =
    byteStringCodec.xmap(bs => Proofs.Script.JS(bs), _.serializedArgs)

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

  implicit val blockBodyV2Codec: Codec[BlockBodyV2] = listCodec[TypedIdentifier]

  implicit val taktikosRegistrationBoxCodec: Codec[Box.Values.TaktikosRegistration] =
    Codec[Proofs.Knowledge.KesProduct].xmap(Box.Values.TaktikosRegistration(_), _.commitment)

  implicit val boxCodec: Codec[Box] = Codec.lazily(???)

  implicit val boxReferenceCodec: Codec[BoxReference] =
    (Codec[DionAddress] :: Codec[BoxNonce](longCodec))
      .xmapc { case address :: nonce :: HNil => (address, nonce) }(t => HList(t._1, t._2))

  implicit val propositionPermanentlyLockedCodec: Codec[Propositions.PermanentlyLocked.type] =
    emptyCodec(Propositions.PermanentlyLocked)

  implicit val propositionsKnowledgeCurve25519Codec: Codec[Propositions.Knowledge.Curve25519] =
    Codec[VerificationKeys.Curve25519].xmap(Propositions.Knowledge.Curve25519, _.key)

  implicit val propositionsKnowledgeEd25519Codec: Codec[Propositions.Knowledge.Ed25519] =
    Codec[VerificationKeys.Ed25519].xmap(Propositions.Knowledge.Ed25519, _.key)

  implicit val propositionsKnowledgeExtendedEd25519Codec: Codec[Propositions.Knowledge.ExtendedEd25519] =
    Codec[VerificationKeys.ExtendedEd25519].xmap(Propositions.Knowledge.ExtendedEd25519, _.key)

  implicit val propositionsKnowledgeHashLockCodec: Codec[Propositions.Knowledge.HashLock] =
    Codec[Digest32].xmap(Propositions.Knowledge.HashLock, _.digest)

  implicit val propositionsCompositionalThresholdCodec: Codec[Propositions.Compositional.Threshold] =
    Codec.lazily(
      (Codec[Int](intCodec) :: Codec[ListSet[Proposition]])
        .xmapc { case threshold :: propositions :: HNil =>
          Propositions.Compositional.Threshold(threshold, propositions)
        }(t => HList(t.threshold, t.propositions))
    )

  implicit val propositionsCompositionalAndCodec: Codec[Propositions.Compositional.And] =
    Codec.lazily(
      (Codec[Proposition] :: Codec[Proposition])
        .xmapc { case a :: b :: HNil => Propositions.Compositional.And(a, b) }(t => HList(t.a, t.b))
    )

  implicit val propositionsCompositionalOrCodec: Codec[Propositions.Compositional.Or] =
    Codec.lazily(
      (Codec[Proposition] :: Codec[Proposition])
        .xmapc { case a :: b :: HNil => Propositions.Compositional.Or(a, b) }(t => HList(t.a, t.b))
    )

  implicit val propositionsCompositionalNotCodec: Codec[Propositions.Compositional.Not] =
    Codec.lazily(Codec[Proposition].xmap(Propositions.Compositional.Not, _.a))

  implicit val propositionsContextualHeightLockCodec: Codec[Propositions.Contextual.HeightLock] =
    uLongCodec.xmap(Propositions.Contextual.HeightLock, _.height)

  implicit val boxLocationCodec: Codec[BoxLocation] =
    discriminated[BoxLocation]
      .by(byteCodec)
      .typecase(0: Byte, emptyCodec(BoxLocations.Input))
      .typecase(1: Byte, emptyCodec(BoxLocations.Output))

  implicit val propositionsContextualRequiredBoxStateCodec: Codec[Propositions.Contextual.RequiredBoxState] =
    Codec.lazily(
      (Codec[BoxLocation] :: Codec[List[(Int, Box)]](listCodec(tupleCodec(intCodec, boxCodec))))
        .xmapc { case location :: boxes :: HNil => Propositions.Contextual.RequiredBoxState(location, boxes) }(t =>
          HList(t.location, t.boxes)
        )
    )

  implicit val propositionsScriptJsCodec: Codec[Propositions.Script.JS] =
    byteStringCodec.xmap(bs => Propositions.Script.JS(Propositions.Script.JS.JSScript(bs)), _.script.value)

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

  implicit val polyOutputCodec: Codec[Transaction.PolyOutput] =
    (Codec[DionAddress] :: Codec[Int128])
      .xmapc { case address :: value :: HNil => Transaction.PolyOutput(address, value) }(t =>
        HList(t.dionAddress, t.value)
      )

  implicit val arbitOutputCodec: Codec[Transaction.ArbitOutput] =
    (Codec[DionAddress] :: Codec[Int128])
      .xmapc { case address :: value :: HNil => Transaction.ArbitOutput(address, value) }(t =>
        HList(t.dionAddress, t.value)
      )

  implicit val assetOutputCodec: Codec[Transaction.AssetOutput] =
    (Codec[DionAddress] :: Codec[Box.Values.Asset])
      .xmapc { case address :: value :: HNil => Transaction.AssetOutput(address, value) }(t =>
        HList(t.dionAddress, t.value)
      )

  implicit val coinOutputCodec: Codec[Transaction.CoinOutput] =
    discriminated[Transaction.CoinOutput]
      .by(byteCodec)
      .typecase(0: Byte, polyOutputCodec)
      .typecase(1: Byte, arbitOutputCodec)
      .typecase(2: Byte, assetOutputCodec)

  implicit val transactionCodec: Codec[Transaction] =
    (
      Codec[ListMap[BoxReference, (Proposition, Proof)]] ::
        Codec[Option[Transaction.PolyOutput]] ::
        Codec[NonEmptyChain[Transaction.CoinOutput]] ::
        Codec[Int128] ::
        Codec[Timestamp](uLongCodec) ::
        Codec[Option[TransactionData]] ::
        Codec[Boolean]
    ).xmapc { case inputs :: feeOutput :: coinOutputs :: fee :: timestamp :: data :: minting :: HNil =>
      Transaction(inputs, feeOutput, coinOutputs, fee, timestamp, data, minting)
    }(t => HList(t.inputs, t.feeOutput, t.coinOutputs, t.fee, t.timestamp, t.data, t.minting))

}

object TetraScodecCodecs extends TetraScodecCodecs
