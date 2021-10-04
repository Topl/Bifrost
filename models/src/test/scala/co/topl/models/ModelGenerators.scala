package co.topl.models

import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Length, Lengths, Ratio, Sized}
import org.scalacheck.Gen

trait ModelGenerators {

  def etaGen: Gen[Eta] =
    genSizedStrictBytes[Lengths.`32`.type]()

  def relativeStakeGen: Gen[Ratio] =
    Gen.chooseNum(1L, 5L).flatMap(denominator => Ratio(1L, denominator))

  def eligibilityCertificateGen: Gen[EligibilityCertificate] =
    for {
      nonceProof <- genSizedStrictBytes[Lengths.`80`.type]().map(Proofs.Signature.VrfEd25519(_))
      testProof  <- genSizedStrictBytes[Lengths.`80`.type]().map(Proofs.Signature.VrfEd25519(_))
      vkVrf      <- ed25519VkGen.map(VerificationKeys.Vrf)
      thresholdEvidence <- genSizedStrictBytes[Lengths.`32`.type]().map(b =>
        Sized.strict[TypedBytes, Lengths.`33`.type](TypedBytes(1: Byte, b.data)).toOption.get
      )
      eta <- etaGen
    } yield EligibilityCertificate(nonceProof, testProof, vkVrf, thresholdEvidence, eta)

  def ed25519VkGen: Gen[VerificationKeys.Ed25519] =
    genSizedStrictBytes[Lengths.`32`.type]().map(VerificationKeys.Ed25519(_))

  def extendedEd25519VkGen: Gen[VerificationKeys.ExtendedEd25519] =
    for {
      ed25519   <- ed25519VkGen
      chainCode <- genSizedStrictBytes[VerificationKeys.ExtendedEd25519.ChainCodeLength]()
    } yield VerificationKeys.ExtendedEd25519(ed25519, chainCode)

  def witnessGen: Gen[Seq[VerificationKeys.Ed25519]] =
    Gen.nonEmptyListOf(ed25519VkGen)

  def sumProductGen: Gen[Proofs.Signature.SumProduct] =
    for {
      ecSignature <- genSizedStrictBytes[Proofs.Signature.Ed25519.Length]().map(Proofs.Signature.Ed25519(_))
      vkK         <- ed25519VkGen
      index       <- Gen.chooseNum[Long](0, 100L)
      witness     <- witnessGen
    } yield Proofs.Signature.SumProduct(ecSignature, vkK, index, witness)

  def hdKesProofGen: Gen[Proofs.Signature.HdKes] =
    for {
      i           <- Gen.chooseNum[Long](0, 100L)
      vki         <- genSizedStrictBytes[Lengths.`32`.type]().map(VerificationKeys.Ed25519(_))
      ecSignature <- genSizedStrictBytes[Proofs.Signature.Ed25519.Length]().map(Proofs.Signature.Ed25519(_))
      sigSumJ     <- sumProductGen
      sigSumK     <- sumProductGen
    } yield Proofs.Signature.HdKes(i, vki, ecSignature, sigSumJ, sigSumK)

  def operationalCertificateGen: Gen[OperationalCertificate] =
    for {
      opSig <- hdKesProofGen
      xvkM  <- extendedEd25519VkGen
      slotR <- Gen.chooseNum[Long](0, 100L)
    } yield OperationalCertificate(opSig, xvkM, slotR)

  def taktikosAddressGen: Gen[TaktikosAddress] =
    for {
      paymentVerificationKeyHash <- genSizedStrictBytes[Lengths.`32`.type]()
      stakingVerificationKey     <- genSizedStrictBytes[Lengths.`32`.type]()
      signature                  <- genSizedStrictBytes[Lengths.`64`.type]()
    } yield TaktikosAddress(paymentVerificationKeyHash, stakingVerificationKey, signature)

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
}

object ModelGenerators extends ModelGenerators
