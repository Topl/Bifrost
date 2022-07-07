package co.topl.models

import cats.data.{Chain, NonEmptyChain, NonEmptyList}
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility._
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Gen}
import org.scalactic.anyvals.NonEmptyMap

import scala.collection.immutable.{ListSet, SortedSet}

trait ModelGenerators {

  def nonEmptyChainOf[T](gen: => Gen[T]): Gen[NonEmptyChain[T]] =
    for {
      tail <- Gen.listOf(gen)
    } yield NonEmptyChain.fromNonEmptyList(NonEmptyList(gen.first, tail))

  /**
   * Similar to Gen.nonEmptyListOf, but without the bug associated with:https://github.com/typelevel/scalacheck/issues/372
   */
  def nonEmptyListOf[T](gen: => Gen[T]): Gen[List[T]] =
    nonEmptyChainOf(gen).map(_.toNonEmptyList.toList)

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

  def baseAddressGen: Gen[StakingAddresses.Operator] =
    for {
      poolVK <- ed25519VkGen
    } yield StakingAddresses.Operator(poolVK)

  def stakingAddressGen: Gen[StakingAddress] =
    baseAddressGen

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
    addressGen: Gen[StakingAddresses.Operator] = baseAddressGen
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
    addressGen: Gen[StakingAddresses.Operator] = baseAddressGen
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

  implicit def arbitrarySpendingAddress: Arbitrary[SpendingAddress] =
    Arbitrary(
      for {
        typedEvidence <- typedEvidenceGen
      } yield SpendingAddress(typedEvidence)
    )

  implicit val arbitraryFullAddress: Arbitrary[FullAddress] =
    Arbitrary(
      for {
        prefix   <- networkPrefixGen
        spending <- arbitrarySpendingAddress.arbitrary
        staking  <- stakingAddressGen
        binding  <- arbitraryProofsKnowledgeEd25519.arbitrary
      } yield FullAddress(prefix, spending, staking, binding)
    )

  implicit val arbitraryInt128: Arbitrary[Int128] =
    Arbitrary(Gen.long.map(BigInt(_)).map(Sized.maxUnsafe[BigInt, Lengths.`128`.type](_)))

  val arbitraryPositiveInt128: Arbitrary[Int128] =
    Arbitrary(Gen.posNum[Long].map(BigInt(_)).map(Sized.maxUnsafe[BigInt, Lengths.`128`.type](_)))

  implicit val arbitraryAssetCode: Arbitrary[Box.Values.Asset.Code] =
    Arbitrary(
      for {
        version   <- byteGen
        issuer    <- arbitrarySpendingAddress.arbitrary
        shortName <- latin1DataGen.map(data => Latin1Data.unsafe(data.value.take(8)))
        code = Box.Values.Asset.Code(version, issuer, Sized.maxUnsafe(shortName))
      } yield code
    )

  implicit val arbitraryAssetBox: Arbitrary[Box.Values.Asset] =
    Arbitrary(
      for {
        quantity <- arbitraryPositiveInt128.arbitrary
        code     <- arbitraryAssetCode.arbitrary
        root     <- genSizedStrictBytes[Lengths.`32`.type]()
        metadata <-
          Gen.option(
            latin1DataGen
              .map(data => Latin1Data.unsafe(data.value.take(127)))
              .map(data => Sized.maxUnsafe[Latin1Data, Lengths.`127`.type](data))
          )
        box = Box.Values.Asset(quantity, code, root, metadata)
      } yield box
    )

  implicit val arbitraryPolyBox: Arbitrary[Box.Values.Poly] =
    Arbitrary(arbitraryPositiveInt128.arbitrary.map(Box.Values.Poly))

  implicit val arbitraryArbitBox: Arbitrary[Box.Values.Arbit] =
    Arbitrary(arbitraryPositiveInt128.arbitrary.map(Box.Values.Arbit))

  implicit val arbitraryBoxValue: Arbitrary[Box.Value] =
    Arbitrary(
      Gen.oneOf(
        Gen.const(Box.Values.Empty),
        arbitraryPolyBox.arbitrary,
        arbitraryArbitBox.arbitrary,
        arbitraryAssetBox.arbitrary
      )
    )

  implicit val arbitraryBox: Arbitrary[Box] =
    Arbitrary(
      for {
        evidence <- typedEvidenceGen
        value    <- arbitraryBoxValue.arbitrary
      } yield Box(evidence, value)
    )

  implicit val arbitraryTransactionOutput: Arbitrary[Transaction.Output] =
    Arbitrary(
      for {
        address <- arbitraryFullAddress.arbitrary
        value   <- arbitraryBoxValue.arbitrary
        minting <- Gen.prob(0.05)
      } yield Transaction.Output(address, value, minting)
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

  implicit val arbitraryPropositionsKnowledgeHashLock: Arbitrary[Propositions.Knowledge.Password] =
    Arbitrary(implicitly[Arbitrary[Digest32]].arbitrary.map(Propositions.Knowledge.Password))

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
    Arbitrary(Gen.posNum[Short].flatMap(index => Gen.oneOf(BoxLocations.Input(index), BoxLocations.Output(index))))

  implicit val arbitraryPropositionsContextualRequiredBoxState: Arbitrary[Propositions.Contextual.RequiredBoxState] =
    Arbitrary(
      for {
        boxes <- Gen.nonEmptyListOf(
          Gen.zip(
            arbitraryBox.arbitrary,
            arbitraryBoxLocation.arbitrary
          )
        )
      } yield Propositions.Contextual.RequiredBoxState(boxes)
    )

  implicit val arbitraryTypedIdentifier: Arbitrary[TypedIdentifier] =
    Arbitrary(
      for {
        byte <- byteGen
        data <- arbitraryBytes.arbitrary
      } yield TypedBytes(byte, data)
    )

  implicit val arbitraryProposition: Arbitrary[Proposition] =
    Arbitrary(
      Gen.oneOf(
        implicitly[Arbitrary[Propositions.PermanentlyLocked.type]].arbitrary,
        implicitly[Arbitrary[Propositions.Knowledge.Curve25519]].arbitrary,
        implicitly[Arbitrary[Propositions.Knowledge.Ed25519]].arbitrary,
        implicitly[Arbitrary[Propositions.Knowledge.ExtendedEd25519]].arbitrary,
        implicitly[Arbitrary[Propositions.Knowledge.Password]].arbitrary,
        implicitly[Arbitrary[Propositions.Compositional.Threshold]].arbitrary,
        implicitly[Arbitrary[Propositions.Compositional.And]].arbitrary,
        implicitly[Arbitrary[Propositions.Compositional.Or]].arbitrary,
        implicitly[Arbitrary[Propositions.Compositional.Not]].arbitrary,
        implicitly[Arbitrary[Propositions.Contextual.HeightLock]].arbitrary,
        implicitly[Arbitrary[Propositions.Contextual.RequiredBoxState]].arbitrary
      )
    )

  implicit val arbitraryProofsFalse: Arbitrary[Proofs.Undefined.type] =
    Arbitrary(Gen.const(Proofs.Undefined))

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

  implicit val arbitraryProofsKnowledgeHashLock: Arbitrary[Proofs.Knowledge.Password] =
    Arbitrary(
      for {
        value <- genSizedMaxBytes[Lengths.`256`.type]()
      } yield Proofs.Knowledge.Password(value)
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

  implicit val arbitraryBoxId: Arbitrary[Box.Id] =
    Arbitrary(
      for {
        transactionId          <- arbitraryTypedIdentifier.arbitrary
        transactionOutputIndex <- Gen.posNum[Short]
      } yield Box.Id(transactionId, transactionOutputIndex)
    )

  implicit val arbitraryTransactionInput: Arbitrary[Transaction.Input] =
    Arbitrary(
      for {
        boxId       <- arbitraryBoxId.arbitrary
        proposition <- arbitraryProposition.arbitrary
        proof       <- arbitraryProof.arbitrary
        value       <- arbitraryBoxValue.arbitrary
      } yield Transaction.Input(boxId, proposition, proof, value)
    )

  implicit val arbitraryTransactionUnprovenInput: Arbitrary[Transaction.Unproven.Input] =
    Arbitrary(
      for {
        boxId       <- arbitraryBoxId.arbitrary
        proposition <- arbitraryProposition.arbitrary
        value       <- arbitraryBoxValue.arbitrary
      } yield Transaction.Unproven.Input(boxId, proposition, value)
    )

  implicit val arbitraryTransactionChronology: Arbitrary[Transaction.Chronology] =
    Arbitrary(
      for {
        creation    <- Gen.chooseNum[Long](0L, 100000L)
        minimumSlot <- Gen.chooseNum[Slot](0L, 100000L)
        maximumSlot <- Gen.chooseNum[Slot](0L, 100000L)
      } yield Transaction.Chronology(creation, minimumSlot, maximumSlot)
    )

  implicit val arbitraryUnprovenTransaction: Arbitrary[Transaction.Unproven] =
    Arbitrary(
      for {
        inputs <-
          Gen
            .chooseNum[Int](1, 10)
            .flatMap(count =>
              Gen
                .listOfN(count, arbitraryTransactionUnprovenInput.arbitrary)
                .map(Chain.fromSeq)
            )
        outputs <-
          Gen
            .chooseNum[Int](1, 10)
            .flatMap(count =>
              Gen
                .listOfN(count, arbitraryTransactionOutput.arbitrary)
                .map(Chain.fromSeq)
            )
        chronology <- arbitraryTransactionChronology.arbitrary
        data = None
      } yield Transaction.Unproven(inputs, outputs, chronology, data)
    )

  implicit val arbitraryTransaction: Arbitrary[Transaction] =
    Arbitrary(
      for {
        inputs <-
          Gen
            .chooseNum[Int](1, 4)
            .flatMap(count =>
              Gen
                .listOfN(count, arbitraryTransactionInput.arbitrary)
                .map(Chain.fromSeq)
            )
        outputs <-
          Gen
            .chooseNum[Int](1, 4)
            .flatMap(count =>
              Gen
                .listOfN(count, arbitraryTransactionOutput.arbitrary)
                .map(Chain.fromSeq)
            )
        chronology <- arbitraryTransactionChronology.arbitrary
        data = None
      } yield Transaction(inputs, outputs, chronology, data)
    )

  implicit val arbitrarySlotId: Arbitrary[SlotId] =
    Arbitrary(
      for {
        slot    <- Gen.posNum[Long]
        blockId <- arbitraryTypedIdentifier.arbitrary
      } yield SlotId(slot, blockId)
    )

  implicit val arbitraryRho: Arbitrary[Rho] =
    Arbitrary(
      genSizedStrictBytes[Lengths.`64`.type]().map(Rho(_))
    )

  implicit val arbitrarySlotData: Arbitrary[SlotData] =
    Arbitrary(
      for {
        slotId       <- arbitrarySlotId.arbitrary
        parentSlotId <- arbitrarySlotId.arbitrary
        rho          <- arbitraryRho.arbitrary
        eta          <- etaGen
        height       <- Gen.posNum[Long]
      } yield SlotData(slotId, parentSlotId, rho, eta, height)
    )

  implicit val arbitraryHeader: Arbitrary[BlockHeaderV2] =
    Arbitrary(headerGen())

  implicit val arbitraryBody: Arbitrary[BlockBodyV2] =
    Arbitrary(Gen.listOf(arbitraryTypedIdentifier.arbitrary).map(ListSet.empty[TypedIdentifier] ++ _))

  implicit val arbitraryEta: Arbitrary[Eta] =
    Arbitrary(etaGen)

  implicit val arbitraryTaktikosAddress: Arbitrary[StakingAddress] =
    Arbitrary(stakingAddressGen)

  implicit class GenHelper[T](gen: Gen[T]) {
    def first: T = gen.pureApply(Gen.Parameters.default, Seed.random())
  }
}

object ModelGenerators extends ModelGenerators
