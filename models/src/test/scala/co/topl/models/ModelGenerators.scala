package co.topl.models

import cats.data.NonEmptyChain
import co.topl.models.Transaction.{ArbitOutput, AssetOutput, PolyOutput}
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{KesBinaryTree, Length, Lengths, Ratio, Sized}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.rng.Seed

import scala.collection.immutable.ListMap

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

  def kesProductProofGen: Gen[Proofs.Knowledge.KesProduct] =
    for {
      superSignature <- kesSumProofGen
      subSignature   <- kesSumProofGen
      subRoot        <- genSizedStrictBytes[Lengths.`32`.type]()
    } yield Proofs.Knowledge.KesProduct(superSignature, subSignature, subRoot)

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

  implicit val arbitraryAssetCode: Arbitrary[Box.Values.Asset.Code] =
    Arbitrary(
      for {
        version   <- byteGen
        issuer    <- arbitraryDionAddress.arbitrary
        shortName <- latin1DataGen.map(data => Latin1Data.unsafe(data.value.take(8)))
        code = Box.Values.Asset.Code(version, issuer, Sized.maxUnsafe(shortName))
      } yield code
    )

  implicit val arbitraryAssetBox: Arbitrary[Box.Values.Asset] =
    Arbitrary(
      for {
        quantity <- arbitraryInt128.arbitrary
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

  implicit val arbitraryPolyOutput: Arbitrary[PolyOutput] =
    Arbitrary(
      arbitraryDionAddress.arbitrary.flatMap(a => arbitraryInt128.arbitrary.map(v => Transaction.PolyOutput(a, v)))
    )

  implicit val arbitraryArbitOutput: Arbitrary[ArbitOutput] =
    Arbitrary(
      arbitraryDionAddress.arbitrary.flatMap(a => arbitraryInt128.arbitrary.map(v => Transaction.ArbitOutput(a, v)))
    )

  implicit val arbitraryAssetOutput: Arbitrary[AssetOutput] =
    Arbitrary(
      for {
        address <- arbitraryDionAddress.arbitrary
        value   <- arbitraryAssetBox.arbitrary
      } yield AssetOutput(address, value)
    )

  implicit val arbitraryUnprovenTransaction: Arbitrary[Transaction.Unproven] =
    Arbitrary(
      for {
        inputs <- Gen.nonEmptyContainerOf[List, BoxReference](
          arbitraryDionAddress.arbitrary.flatMap(a => Gen.long.map(l => (a, l)))
        )
        feeOutput   <- Gen.option(arbitraryPolyOutput.arbitrary)
        coinOutputs <- Gen.nonEmptyListOf(arbitraryPolyOutput.arbitrary).map(NonEmptyChain.fromSeq(_).get)
        fee         <- arbitraryInt128.arbitrary
        timestamp   <- Gen.chooseNum[Long](0L, 100000L)
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
              Gen
                .zip(
                  genSizedStrictBytes[VerificationKeys.Ed25519.Length]()
                    .map(x => Propositions.Knowledge.Ed25519(VerificationKeys.Ed25519(x)): Proposition),
                  genSizedStrictBytes[Proofs.Knowledge.Ed25519.Length]().map(x => Proofs.Knowledge.Ed25519(x): Proof)
                )
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
