package co.topl.models

import cats.data.NonEmptyChain
import co.topl.models.Proofs.Knowledge.KesSum
import co.topl.models.Transaction.PolyOutput
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Length, Lengths, Ratio, Sized}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.rng.Seed

trait ModelGenerators {

  def etaGen: Gen[Eta] =
    genSizedStrictBytes[Lengths.`32`.type]()

  def relativeStakeGen: Gen[Ratio] =
    Gen.chooseNum(1L, 5L).flatMap(denominator => Ratio(1L, denominator))

  def eligibilityCertificateGen: Gen[EligibilityCertificate] =
    for {
      nonceProof        <- genSizedStrictBytes[Lengths.`80`.type]().map(Proofs.Knowledge.VrfEd25519(_))
      testProof         <- genSizedStrictBytes[Lengths.`80`.type]().map(Proofs.Knowledge.VrfEd25519(_))
      vkVrf             <- genSizedStrictBytes[Lengths.`32`.type]().map(VerificationKeys.VrfEd25519(_))
      thresholdEvidence <- genSizedStrictBytes[Lengths.`32`.type]()
      eta               <- etaGen
    } yield EligibilityCertificate(nonceProof, testProof, vkVrf, thresholdEvidence, eta)

  def ed25519VkGen: Gen[VerificationKeys.Ed25519] =
    genSizedStrictBytes[Lengths.`32`.type]().map(VerificationKeys.Ed25519(_))

  def extendedEd25519VkGen: Gen[VerificationKeys.ExtendedEd25519] =
    for {
      ed25519   <- ed25519VkGen
      chainCode <- genSizedStrictBytes[VerificationKeys.ExtendedEd25519.ChainCodeLength]()
    } yield VerificationKeys.ExtendedEd25519(ed25519, chainCode)

  def witnessGen: Gen[Vector[Sized.Strict[Bytes, KesSum.DigestLength]]] =
    Gen.nonEmptyContainerOf[Vector, Sized.Strict[Bytes, Lengths.`32`.type]](genSizedStrictBytes[Lengths.`32`.type]())

  def kesSumProofGen: Gen[Proofs.Knowledge.KesSum] =
    for {
      vkK         <- ed25519VkGen
      ecSignature <- genSizedStrictBytes[Proofs.Knowledge.Ed25519.Length]().map(Proofs.Knowledge.Ed25519(_))
      witness     <- witnessGen
    } yield Proofs.Knowledge.KesSum(vkK, ecSignature, witness)

  def kesVKGen: Gen[VerificationKeys.KesProduct] =
    for {
      bytes <- genSizedStrictBytes[Lengths.`32`.type]()
      idx   <- Gen.posNum[Int]
    } yield VerificationKeys.KesProduct(bytes, idx)

  def kesProductProofGen: Gen[Proofs.Knowledge.KesProduct] =
    for {
      superSignature <- kesSumProofGen
      subSignature   <- kesSumProofGen
      subRoot        <- genSizedStrictBytes[Lengths.`32`.type]()
    } yield Proofs.Knowledge.KesProduct(superSignature, subSignature, subRoot)

  def operationalCertificateGen: Gen[OperationalCertificate] =
    for {
      parentVK        <- kesVKGen
      parentSignature <- kesProductProofGen
      childVK         <- ed25519VkGen
      childSignature  <- genSizedStrictBytes[Lengths.`64`.type]().map(Proofs.Knowledge.Ed25519(_))
    } yield OperationalCertificate(parentVK, parentSignature, childVK, childSignature)

  def taktikosAddressGen: Gen[TaktikosAddress] =
    for {
      paymentVKEvidence <- genSizedStrictBytes[Lengths.`32`.type]()
      poolVK            <- genSizedStrictBytes[Lengths.`32`.type]().map(VerificationKeys.Ed25519(_))
      signature         <- genSizedStrictBytes[Lengths.`64`.type]().map(Proofs.Knowledge.Ed25519(_))
    } yield TaktikosAddress(paymentVKEvidence, poolVK, signature)

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
    metadataGen: Gen[Option[Sized.Max[Latin1Data, Lengths.`32`.type]]] = Gen.option(
      Gen
        .containerOfN[Array, Byte](32, Gen.choose[Byte](0, 32))
        .map(Latin1Data(_))
        .map(Sized.max[Latin1Data, Lengths.`32`.type](_).toOption.get)
    ),
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

  implicit val arbitraryPolyOutput: Arbitrary[PolyOutput] =
    Arbitrary(
      arbitraryDionAddress.arbitrary.flatMap(a => arbitraryInt128.arbitrary.map(v => Transaction.PolyOutput(a, v)))
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
        timestamp   <- Gen.chooseNum[Long](0L, 100_000L)
        data = None
        minting = false
      } yield Transaction.Unproven(inputs, feeOutput, coinOutputs, fee, timestamp, data, minting)
    )

  implicit val arbitraryHeader: Arbitrary[BlockHeaderV2] =
    Arbitrary(headerGen())

  implicit class GenHelper[T](gen: Gen[T]) {
    def first: T = gen.pureApply(Gen.Parameters.default, Seed.random())
  }
}

object ModelGenerators extends ModelGenerators
