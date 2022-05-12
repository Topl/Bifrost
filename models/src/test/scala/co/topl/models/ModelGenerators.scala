package co.topl.models

import cats.data.{Chain, NonEmptyChain}
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility._
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen}

import scala.collection.immutable.{ListMap, ListSet}

trait ModelGenerators {

  def etaGen: Gen[Eta] =
    genSizedStrictBytes[Lengths.`32`.type]()

  def bigIntGen: Gen[BigInt] = Gen.long.map(BigInt(_))

  def ratioGen: Gen[Ratio] =
    for {
      n <- bigIntGen
      d <- bigIntGen
    } yield Ratio(n, d)

  def relativeStakeGen: Gen[Ratio] =
    Gen.chooseNum(1L, 5L).flatMap(denominator => Ratio(1L, denominator))

  def latin1DataGen: Gen[Latin1Data] =
    Gen
      .containerOfN[Array, Byte](32, Gen.choose[Byte](0, 32))
      .map(Latin1Data.fromData(_))

  def vkVrfEd25519Gen: Gen[VerificationKeys.VrfEd25519] =
    genSizedStrictBytes[Lengths.`32`.type]().map(VerificationKeys.VrfEd25519(_))

  def proofVrfEd25519Gen: Gen[Proofs.Knowledge.VrfEd25519] =
    genSizedStrictBytes[Lengths.`80`.type]().map(Proofs.Knowledge.VrfEd25519(_))

  def typedEvidenceGen: Gen[TypedEvidence] =
    for {
      prefix   <- byteGen
      evidence <- genSizedStrictBytes[Lengths.`32`.type]()
    } yield TypedEvidence(prefix, evidence)

  def networkPrefixGen: Gen[NetworkPrefix] =
    byteGen.map(NetworkPrefix(_))

  def dionAddressGen: Gen[DionAddress] =
    for {
      prefix        <- networkPrefixGen
      typedEvidence <- typedEvidenceGen
    } yield DionAddress(prefix, typedEvidence)

  def boxReferenceGen: Gen[BoxReference] =
    for {
      address <- dionAddressGen
      nonce   <- Gen.long
    } yield (address, nonce)

  def eligibilityCertificateGen: Gen[EligibilityCertificate] =
    for {
      vrfProof          <- proofVrfEd25519Gen
      vkVrf             <- vkVrfEd25519Gen
      thresholdEvidence <- genSizedStrictBytes[Lengths.`32`.type]()
      eta               <- etaGen
    } yield EligibilityCertificate(vrfProof, vkVrf, thresholdEvidence, eta)

  def ed25519VkGen: Gen[VerificationKeys.Ed25519] =
    genSizedStrictBytes[Lengths.`32`.type]().map(VerificationKeys.Ed25519(_))

  def extendedEd25519VkGen: Gen[VerificationKeys.ExtendedEd25519] =
    for {
      ed25519   <- ed25519VkGen
      chainCode <- genSizedStrictBytes[VerificationKeys.ExtendedEd25519.ChainCodeLength]()
    } yield VerificationKeys.ExtendedEd25519(ed25519, chainCode)

  def vkKesSumGen: Gen[VerificationKeys.KesSum] =
    for {
      bytes <- genSizedStrictBytes[VerificationKeys.KesSum.Length]()
      step  <- Gen.posNum[Int]
    } yield VerificationKeys.KesSum(bytes, step)

  def witnessGen: Gen[Vector[Sized.Strict[Bytes, Proofs.Knowledge.KesSum.DigestLength]]] =
    Gen.nonEmptyContainerOf[Vector, Sized.Strict[Bytes, Lengths.`32`.type]](genSizedStrictBytes[Lengths.`32`.type]())

  def kesSumProofGen: Gen[Proofs.Knowledge.KesSum] =
    for {
      vkK         <- ed25519VkGen
      ecSignature <- ed25519ProofGen
      witness     <- witnessGen
    } yield Proofs.Knowledge.KesSum(vkK, ecSignature, witness)

  def curve25519ProofGen: Gen[Proofs.Knowledge.Curve25519] =
    genSizedStrictBytes[Proofs.Knowledge.Curve25519.Length]().map(Proofs.Knowledge.Curve25519(_))

  def ed25519ProofGen: Gen[Proofs.Knowledge.Ed25519] =
    genSizedStrictBytes[Proofs.Knowledge.Ed25519.Length]().map(Proofs.Knowledge.Ed25519(_))

  def kesProductProofGen: Gen[Proofs.Knowledge.KesProduct] =
    for {
      superSignature <- kesSumProofGen
      subSignature   <- kesSumProofGen
      subRoot        <- genSizedStrictBytes[Lengths.`32`.type]()
    } yield Proofs.Knowledge.KesProduct(superSignature, subSignature, subRoot)

  def kesVKGen: Gen[VerificationKeys.KesProduct] =
    for {
      bytes <- genSizedStrictBytes[Lengths.`32`.type]()
      idx   <- Gen.posNum[Int]
    } yield VerificationKeys.KesProduct(bytes, idx)

  def skVrfEd25519Gen: Gen[SecretKeys.VrfEd25519] =
    genSizedStrictBytes[SecretKeys.VrfEd25519.Length]().map(SecretKeys.VrfEd25519(_))

  def kesBinaryTreeGen: Gen[KesBinaryTree] =
    Gen.chooseNum[Int](0, 2).flatMap {
      case 0 =>
        for {
          seed         <- Gen.containerOfN[Array, Byte](32, byteGen)
          witnessLeft  <- Gen.containerOfN[Array, Byte](32, byteGen)
          witnessRight <- Gen.containerOfN[Array, Byte](32, byteGen)
          left         <- kesBinaryTreeGen
          right        <- kesBinaryTreeGen
        } yield KesBinaryTree.MerkleNode(seed, witnessLeft, witnessRight, left, right)
      case 1 =>
        for {
          sk <- Gen.containerOfN[Array, Byte](32, byteGen)
          vk <- Gen.containerOfN[Array, Byte](32, byteGen)
        } yield KesBinaryTree.SigningLeaf(sk, vk)
      case 2 => Gen.const(KesBinaryTree.Empty())
    }

  def kesSumSKGen: Gen[SecretKeys.KesSum] =
    for {
      tree   <- kesBinaryTreeGen
      offset <- Gen.long
    } yield SecretKeys.KesSum(tree, offset)

  def kesProductSKGen: Gen[SecretKeys.KesProduct] =
    for {
      superTree   <- kesBinaryTreeGen
      subTree     <- kesBinaryTreeGen
      nextSubSeed <- Gen.containerOfN[Array, Byte](32, byteGen)
      signature   <- kesSumProofGen
      offset      <- Gen.long
    } yield SecretKeys.KesProduct(superTree, subTree, nextSubSeed, signature, offset)

  def partialOperationalCertificateGen: Gen[BlockHeaderV2.Unsigned.PartialOperationalCertificate] =
    for {
      parentVK        <- kesVKGen
      parentSignature <- kesProductProofGen
      childVK         <- ed25519VkGen
    } yield BlockHeaderV2.Unsigned.PartialOperationalCertificate(parentVK, parentSignature, childVK)

  def operationalCertificateGen: Gen[OperationalCertificate] =
    for {
      parentVK        <- kesVKGen
      parentSignature <- kesProductProofGen
      childVK         <- ed25519VkGen
      childSignature  <- ed25519ProofGen
    } yield OperationalCertificate(parentVK, parentSignature, childVK, childSignature)

  def taktikosAddressGen: Gen[TaktikosAddress] =
    for {
      paymentVKEvidence <- genSizedStrictBytes[Lengths.`32`.type]()
      poolVK            <- ed25519VkGen
      signature         <- ed25519ProofGen
    } yield TaktikosAddress(paymentVKEvidence, poolVK, signature)

  def unsignedHeaderGen(
    parentHeaderIdGen: Gen[TypedIdentifier] =
      genSizedStrictBytes[Lengths.`32`.type]().map(sized => TypedBytes(IdentifierTypes.Block.HeaderV2, sized.data)),
    parentSlotGen:             Gen[Slot] = Gen.chooseNum(0L, 50L),
    txRootGen:                 Gen[TxRoot] = genSizedStrictBytes[Lengths.`32`.type](),
    bloomFilterGen:            Gen[BloomFilter] = genSizedStrictBytes[Lengths.`256`.type](),
    timestampGen:              Gen[Timestamp] = Gen.chooseNum(0L, 50L),
    heightGen:                 Gen[Long] = Gen.chooseNum(0L, 20L),
    slotGen:                   Gen[Slot] = Gen.chooseNum(0L, 50L),
    eligibilityCertificateGen: Gen[EligibilityCertificate] = eligibilityCertificateGen,
    partialOperationalCertificateGen: Gen[BlockHeaderV2.Unsigned.PartialOperationalCertificate] =
      partialOperationalCertificateGen,
    metadataGen: Gen[Option[Sized.Max[Latin1Data, Lengths.`32`.type]]] =
      Gen.option(latin1DataGen.map(Sized.maxUnsafe[Latin1Data, Lengths.`32`.type](_))),
    addressGen: Gen[TaktikosAddress] = taktikosAddressGen
  ): Gen[BlockHeaderV2.Unsigned] =
    for {
      parentHeaderID <- parentHeaderIdGen
      parentSlot     <- parentSlotGen
      txRoot         <- txRootGen
      bloomFilter    <- bloomFilterGen
      timestamp      <- timestampGen
      height         <- heightGen
      slot           <- slotGen
      vrfCertificate <- eligibilityCertificateGen
      kesCertificate <- partialOperationalCertificateGen
      metadata       <- metadataGen
      address        <- addressGen
    } yield BlockHeaderV2.Unsigned(
      parentHeaderID,
      parentSlot,
      txRoot,
      bloomFilter,
      timestamp,
      height,
      slot,
      vrfCertificate,
      kesCertificate,
      metadata,
      address
    )

  def headerGen(
    parentHeaderIdGen: Gen[TypedIdentifier] =
      genSizedStrictBytes[Lengths.`32`.type]().map(sized => TypedBytes(IdentifierTypes.Block.HeaderV2, sized.data)),
    parentSlotGen:             Gen[Slot] = Gen.chooseNum(0L, 50L),
    txRootGen:                 Gen[TxRoot] = genSizedStrictBytes[Lengths.`32`.type](),
    bloomFilterGen:            Gen[BloomFilter] = genSizedStrictBytes[Lengths.`256`.type](),
    timestampGen:              Gen[Timestamp] = Gen.chooseNum(0L, 50L),
    heightGen:                 Gen[Long] = Gen.chooseNum(0L, 20L),
    slotGen:                   Gen[Slot] = Gen.chooseNum(0L, 50L),
    eligibilityCertificateGen: Gen[EligibilityCertificate] = eligibilityCertificateGen,
    operationalCertificateGen: Gen[OperationalCertificate] = operationalCertificateGen,
    metadataGen: Gen[Option[Sized.Max[Latin1Data, Lengths.`32`.type]]] =
      Gen.option(latin1DataGen.map(Sized.maxUnsafe[Latin1Data, Lengths.`32`.type](_))),
    addressGen: Gen[TaktikosAddress] = taktikosAddressGen
  ): Gen[BlockHeaderV2] =
    for {
      parentHeaderID <- parentHeaderIdGen
      parentSlot     <- parentSlotGen
      txRoot         <- txRootGen
      bloomFilter    <- bloomFilterGen
      timestamp      <- timestampGen
      height         <- heightGen
      slot           <- slotGen
      vrfCertificate <- eligibilityCertificateGen
      kesCertificate <- operationalCertificateGen
      metadata       <- metadataGen
      address        <- addressGen
    } yield BlockHeaderV2(
      parentHeaderID,
      parentSlot,
      txRoot,
      bloomFilter,
      timestamp,
      height,
      slot,
      vrfCertificate,
      kesCertificate,
      metadata,
      address
    )

  def byteGen: Gen[Byte] = Gen.choose[Byte](Byte.MinValue, Byte.MaxValue)

  def genSizedMaxBytes[L <: Length](
    byteGen:    Gen[Byte] = Gen.choose[Byte](0, 32)
  )(implicit l: L): Gen[Sized.Max[Bytes, L]] =
    Gen
      .containerOfN[Array, Byte](l.value, byteGen)
      .map(Bytes(_))
      .map(Sized.max[Bytes, L](_).toOption.get)

  def genSizedStrictBytes[L <: Length](
    byteGen:    Gen[Byte] = Gen.choose[Byte](0, 32)
  )(implicit l: L): Gen[Sized.Strict[Bytes, L]] =
    Gen
      .containerOfN[Array, Byte](l.value, byteGen)
      .map(Bytes(_))
      .map(Sized.strict[Bytes, L](_).toOption.get)

  implicit val arbitraryBytes: Arbitrary[Bytes] =
    Arbitrary(implicitly[Arbitrary[Array[Byte]]].arbitrary.map(Bytes(_)))

  implicit val arbitraryCurve25519VK: Arbitrary[VerificationKeys.Curve25519] =
    Arbitrary(
      genSizedStrictBytes[VerificationKeys.Curve25519.Length]().map(VerificationKeys.Curve25519(_))
    )

  implicit val arbitraryEd25519VK: Arbitrary[VerificationKeys.Ed25519] =
    Arbitrary(
      genSizedStrictBytes[VerificationKeys.Ed25519.Length]().map(VerificationKeys.Ed25519(_))
    )

  implicit val arbitraryExtendedEd25519VK: Arbitrary[VerificationKeys.ExtendedEd25519] =
    Arbitrary(extendedEd25519VkGen)

  implicit val arbitraryCurve25519SK: Arbitrary[SecretKeys.Curve25519] =
    Arbitrary(
      genSizedStrictBytes[SecretKeys.Curve25519.Length]().map(SecretKeys.Curve25519(_))
    )

  implicit val arbitraryEdSK: Arbitrary[SecretKeys.Ed25519] =
    Arbitrary(
      genSizedStrictBytes[SecretKeys.Ed25519.Length]().map(SecretKeys.Ed25519(_))
    )

  implicit val arbitraryKesProductSK: Arbitrary[SecretKeys.KesProduct] =
    Arbitrary(kesProductSKGen)

  implicit val arbitraryExtendedEdSK: Arbitrary[SecretKeys.ExtendedEd25519] =
    Arbitrary(
      for {
        l <- genSizedStrictBytes[SecretKeys.ExtendedEd25519.LeftLength]()
        r <- genSizedStrictBytes[SecretKeys.ExtendedEd25519.RightLength]()
        c <- genSizedStrictBytes[SecretKeys.ExtendedEd25519.ChainCodeLength]()
      } yield SecretKeys.ExtendedEd25519(l, r, c)
    )

  implicit def arbitraryDionAddress: Arbitrary[DionAddress] =
    Arbitrary(
      for {
        typePrefix <- Gen.chooseNum[Byte](0, Byte.MaxValue)
        evidence   <- genSizedStrictBytes[Lengths.`32`.type]()
      } yield DionAddress(NetworkPrefix(1: Byte), TypedEvidence(typePrefix, evidence))
    )

  implicit val arbitraryInt128: Arbitrary[Int128] =
    Arbitrary(Gen.long.map(BigInt(_)).map(Sized.maxUnsafe[BigInt, Lengths.`128`.type](_)))

  implicit val arbitraryPositiveInt128: Arbitrary[Int128] =
    Arbitrary(Gen.posNum[Long].map(Sized.maxUnsafe[BigInt, Lengths.`128`.type](_)))

  implicit val arbitraryAssetCode: Arbitrary[Box.Values.Asset.Code] =
    Arbitrary(
      for {
        version   <- Gen.const(1.toByte)
        issuer    <- arbitraryDionAddress.arbitrary
        shortName <- latin1DataGen.map(data => Latin1Data.unsafe(data.value.take(8)))
        code = Box.Values.Asset.Code(version, issuer, Sized.maxUnsafe(shortName))
      } yield code
    )

  implicit val arbitraryAssetBox: Arbitrary[Box.Values.Asset] =
    Arbitrary(
      for {
        quantity <- arbitraryPositiveInt128.arbitrary
        code     <- arbitraryAssetCode.arbitrary
        root     <- genSizedStrictBytes[Lengths.`32`.type]().map(_.data)
        metadata <-
          Gen.option(
            latin1DataGen
              .map(data => Latin1Data.unsafe(data.value.take(127)))
              .map(data => Sized.maxUnsafe[Latin1Data, Lengths.`127`.type](data))
          )
        box = Box.Values.Asset(quantity, code, root, metadata)
      } yield box
    )

  implicit val arbitraryBoxValue: Arbitrary[Box.Value] =
    Arbitrary(
      Gen.oneOf(
        Gen.const(Box.Values.Empty),
        arbitraryInt128.arbitrary.map(Box.Values.Poly),
        arbitraryInt128.arbitrary.map(Box.Values.Arbit),
        arbitraryAssetBox.arbitrary
      )
    )

  implicit val arbitraryBox: Arbitrary[Box] =
    Arbitrary(
      for {
        evidence <- typedEvidenceGen
        nonce    <- Gen.long
        value    <- arbitraryBoxValue.arbitrary
      } yield Box(evidence, nonce, value)
    )

  implicit val arbitraryPolyOutput: Arbitrary[Transaction.PolyOutput] =
    Arbitrary(
      arbitraryDionAddress.arbitrary.flatMap(a => arbitraryInt128.arbitrary.map(v => Transaction.PolyOutput(a, v)))
    )

  implicit val arbitraryArbitOutput: Arbitrary[Transaction.ArbitOutput] =
    Arbitrary(
      arbitraryDionAddress.arbitrary.flatMap(a =>
        arbitraryPositiveInt128.arbitrary.map(v => Transaction.ArbitOutput(a, v))
      )
    )

  implicit val arbitraryAssetOutput: Arbitrary[Transaction.AssetOutput] =
    Arbitrary(
      for {
        address <- arbitraryDionAddress.arbitrary
        value   <- arbitraryAssetBox.arbitrary
      } yield Transaction.AssetOutput(address, value)
    )

  implicit val arbitraryCoinOutput: Arbitrary[Transaction.CoinOutput] =
    Arbitrary(
      Gen.oneOf(
        arbitraryPolyOutput.arbitrary,
        arbitraryArbitOutput.arbitrary,
        arbitraryAssetOutput.arbitrary
      )
    )

  implicit val arbitraryPropositionsPermanentlyLocked: Arbitrary[Propositions.PermanentlyLocked.type] =
    Arbitrary(Gen.const(Propositions.PermanentlyLocked))

  implicit val arbitraryPropositionsKnowledgeCurve25519: Arbitrary[Propositions.Knowledge.Curve25519] =
    Arbitrary(arbitraryCurve25519VK.arbitrary.map(Propositions.Knowledge.Curve25519))

  implicit val arbitraryPropositionsKnowledgeEd25519: Arbitrary[Propositions.Knowledge.Ed25519] =
    Arbitrary(arbitraryEd25519VK.arbitrary.map(Propositions.Knowledge.Ed25519))

  implicit val arbitraryPropositionsKnowledgeExtendedEd25519: Arbitrary[Propositions.Knowledge.ExtendedEd25519] =
    Arbitrary(arbitraryExtendedEd25519VK.arbitrary.map(Propositions.Knowledge.ExtendedEd25519))

  implicit def arbitraryStrictSizedBytes[L <: Length](implicit l: L): Arbitrary[Sized.Strict[Bytes, L]] =
    Arbitrary(genSizedStrictBytes[L]())

  implicit val arbitraryPropositionsKnowledgeHashLock: Arbitrary[Propositions.Knowledge.HashLock] =
    Arbitrary(implicitly[Arbitrary[Digest32]].arbitrary.map(Propositions.Knowledge.HashLock))

  implicit val arbitraryPropositionsCompositionalThreshold: Arbitrary[Propositions.Compositional.Threshold] =
    Arbitrary(
      Gen
        .chooseNum[Int](1, 5)
        .flatMap(threshold =>
          Gen
            .containerOfN[ListSet, Proposition](threshold + 1, Gen.delay(arbitraryProposition.arbitrary))
            .map(propositions => Propositions.Compositional.Threshold(threshold, propositions))
        )
    )

  implicit val arbitraryPropositionsCompositionalAnd: Arbitrary[Propositions.Compositional.And] =
    Arbitrary(
      for {
        a <- Gen.delay(arbitraryProposition.arbitrary)
        b <- Gen.delay(arbitraryProposition.arbitrary)
      } yield Propositions.Compositional.And(a, b)
    )

  implicit val arbitraryPropositionsCompositionalOr: Arbitrary[Propositions.Compositional.Or] =
    Arbitrary(
      for {
        a <- Gen.delay(arbitraryProposition.arbitrary)
        b <- Gen.delay(arbitraryProposition.arbitrary)
      } yield Propositions.Compositional.Or(a, b)
    )

  implicit val arbitraryPropositionsCompositionalNot: Arbitrary[Propositions.Compositional.Not] =
    Arbitrary(Gen.delay(arbitraryProposition.arbitrary).map(Propositions.Compositional.Not))

  implicit val arbitraryPropositionsContextualHeightLock: Arbitrary[Propositions.Contextual.HeightLock] =
    Arbitrary(Gen.posNum[Long].map(Propositions.Contextual.HeightLock))

  implicit val arbitraryBoxLocation: Arbitrary[BoxLocation] =
    Arbitrary(Gen.oneOf(BoxLocations.Input, BoxLocations.Output))

  implicit val arbitraryPropositionsContextualRequiredBoxState: Arbitrary[Propositions.Contextual.RequiredBoxState] =
    Arbitrary(
      for {
        location <- arbitraryBoxLocation.arbitrary
        boxes <- Gen.nonEmptyListOf(
          Gen.zip(
            Gen.chooseNum[Int](0, 5),
            arbitraryBox.arbitrary
          )
        )
      } yield Propositions.Contextual.RequiredBoxState(location, boxes)
    )

  implicit val arbitraryPropositionsScriptJs: Arbitrary[Propositions.Script.JS] =
    Arbitrary(
      Gen.asciiPrintableStr
        .map(Propositions.Script.JS.JSScript(_))
        .map(Propositions.Script.JS(_))
    )

  implicit val arbitraryProposition: Arbitrary[Proposition] =
    Arbitrary(
      Gen.oneOf(
        implicitly[Arbitrary[Propositions.PermanentlyLocked.type]].arbitrary,
        implicitly[Arbitrary[Propositions.Knowledge.Curve25519]].arbitrary,
        implicitly[Arbitrary[Propositions.Knowledge.Ed25519]].arbitrary,
        implicitly[Arbitrary[Propositions.Knowledge.ExtendedEd25519]].arbitrary,
        implicitly[Arbitrary[Propositions.Knowledge.HashLock]].arbitrary,
        implicitly[Arbitrary[Propositions.Compositional.Threshold]].arbitrary,
        implicitly[Arbitrary[Propositions.Compositional.And]].arbitrary,
        implicitly[Arbitrary[Propositions.Compositional.Or]].arbitrary,
        implicitly[Arbitrary[Propositions.Compositional.Not]].arbitrary,
        implicitly[Arbitrary[Propositions.Contextual.HeightLock]].arbitrary,
        implicitly[Arbitrary[Propositions.Contextual.RequiredBoxState]].arbitrary,
        implicitly[Arbitrary[Propositions.Script.JS]].arbitrary
      )
    )

  implicit val arbitraryProofsFalse: Arbitrary[Proofs.False.type] =
    Arbitrary(Gen.const(Proofs.False))

  implicit val arbitraryProofsKnowledgeCurve25519: Arbitrary[Proofs.Knowledge.Curve25519] =
    Arbitrary(curve25519ProofGen)

  implicit val arbitraryProofsKnowledgeEd25519: Arbitrary[Proofs.Knowledge.Ed25519] =
    Arbitrary(ed25519ProofGen)

  implicit val arbitraryProofsKnowledgeVrfEd25519: Arbitrary[Proofs.Knowledge.VrfEd25519] =
    Arbitrary(proofVrfEd25519Gen)

  implicit val arbitraryProofsKnowledgeKesSum: Arbitrary[Proofs.Knowledge.KesSum] =
    Arbitrary(kesSumProofGen)

  implicit val arbitraryProofsKnowledgeKesProduct: Arbitrary[Proofs.Knowledge.KesProduct] =
    Arbitrary(kesProductProofGen)

  implicit val arbitraryProofsKnowledgeHashLock: Arbitrary[Proofs.Knowledge.HashLock] =
    Arbitrary(
      for {
        salt  <- genSizedStrictBytes[Lengths.`32`.type]()
        value <- byteGen
      } yield Proofs.Knowledge.HashLock(salt, value)
    )

  implicit val arbitraryProofsCompositionalThreshold: Arbitrary[Proofs.Compositional.Threshold] =
    Arbitrary(
      Gen
        .chooseNum[Int](1, 4)
        .flatMap(count => Gen.listOfN(count, Gen.delay(arbitraryProof.arbitrary)))
        .map(Proofs.Compositional.Threshold)
    )

  implicit val arbitraryProofsCompositionalAnd: Arbitrary[Proofs.Compositional.And] =
    Arbitrary(
      for {
        a <- Gen.delay(arbitraryProof.arbitrary)
        b <- Gen.delay(arbitraryProof.arbitrary)
      } yield Proofs.Compositional.And(a, b)
    )

  implicit val arbitraryProofsCompositionalOr: Arbitrary[Proofs.Compositional.Or] =
    Arbitrary(
      for {
        a <- Gen.delay(arbitraryProof.arbitrary)
        b <- Gen.delay(arbitraryProof.arbitrary)
      } yield Proofs.Compositional.Or(a, b)
    )

  implicit val arbitraryProofsCompositionalNot: Arbitrary[Proofs.Compositional.Not] =
    Arbitrary(
      Gen.delay(arbitraryProof.arbitrary).map(Proofs.Compositional.Not)
    )

  implicit val arbitraryProofsContextualHeightLock: Arbitrary[Proofs.Contextual.HeightLock] =
    Arbitrary(
      Gen.const(Proofs.Contextual.HeightLock())
    )

  implicit val arbitraryProofsContextualRequiredBoxState: Arbitrary[Proofs.Contextual.RequiredBoxState] =
    Arbitrary(
      Gen.const(Proofs.Contextual.RequiredBoxState())
    )

  implicit val arbitraryProofsScriptJs: Arbitrary[Proofs.Script.JS] =
    Arbitrary(
      Gen.asciiStr.map(Proofs.Script.JS(_))
    )

  implicit val arbitraryProof: Arbitrary[Proof] =
    Arbitrary(
      Gen.oneOf(
        arbitraryProofsFalse.arbitrary,
        arbitraryProofsKnowledgeCurve25519.arbitrary,
        arbitraryProofsKnowledgeEd25519.arbitrary,
        arbitraryProofsKnowledgeVrfEd25519.arbitrary,
        arbitraryProofsKnowledgeHashLock.arbitrary,
        arbitraryProofsCompositionalThreshold.arbitrary,
        arbitraryProofsCompositionalAnd.arbitrary,
        arbitraryProofsCompositionalOr.arbitrary,
        arbitraryProofsCompositionalNot.arbitrary,
        arbitraryProofsContextualHeightLock.arbitrary,
        arbitraryProofsContextualRequiredBoxState.arbitrary,
        arbitraryProofsScriptJs.arbitrary
      )
    )

  implicit val arbitraryUnprovenTransaction: Arbitrary[Transaction.Unproven] =
    Arbitrary(
      for {
        inputs <- Gen.nonEmptyContainerOf[List, BoxReference](
          arbitraryDionAddress.arbitrary.flatMap(a => Gen.long.map(l => (a, l)))
        )
        feeOutput <- Gen.option(arbitraryPolyOutput.arbitrary)
        coinOutputs <- Gen
          .nonEmptyListOf(arbitraryCoinOutput.arbitrary)
          .map(Chain.fromSeq)
          .map(NonEmptyChain.fromChainUnsafe)
        fee       <- arbitraryInt128.arbitrary
        timestamp <- Gen.chooseNum[Long](0L, 100000L)
        data = None
        minting = false
      } yield Transaction.Unproven(inputs, feeOutput, coinOutputs, fee, timestamp, data, minting)
    )

  implicit val arbitraryTransaction: Arbitrary[Transaction] =
    Arbitrary(
      for {
        unprovenTx <- arbitraryUnprovenTransaction.arbitrary
        provenInputs <-
          Gen
            .listOfN(
              unprovenTx.inputs.length,
              Gen.zip(arbitraryProposition.arbitrary, arbitraryProof.arbitrary)
            )
            .map(pairs => unprovenTx.inputs.zip(pairs))
        tx = Transaction(
          inputs = ListMap(provenInputs: _*),
          feeOutput = unprovenTx.feeOutput,
          coinOutputs = unprovenTx.coinOutputs,
          fee = unprovenTx.fee,
          timestamp = unprovenTx.timestamp,
          data = unprovenTx.data,
          minting = unprovenTx.minting
        )
      } yield tx
    )

  implicit val arbitraryHeader: Arbitrary[BlockHeaderV2] =
    Arbitrary(headerGen())

  implicit val arbitraryEta: Arbitrary[Eta] =
    Arbitrary(etaGen)

  implicit val arbitraryTaktikosAddress: Arbitrary[TaktikosAddress] =
    Arbitrary(taktikosAddressGen)

  implicit class GenHelper[T](gen: Gen[T]) {
    def first: T = gen.pureApply(Gen.Parameters.default, Seed.random())
  }
}

object ModelGenerators extends ModelGenerators
