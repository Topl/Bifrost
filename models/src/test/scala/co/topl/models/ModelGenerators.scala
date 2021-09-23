package co.topl.models

import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Length, Lengths, Ratio, Sized}
import org.scalacheck.Gen
import org.scalacheck.Gen.posNum

trait ModelGenerators {

  def etaGen: Gen[Eta] =
    genSizedStrictBytes[Lengths.`32`.type]()

  def relativeStakeGen: Gen[Ratio] =
    Gen.chooseNum(1L, 5L).flatMap(denominator => Ratio(1L, denominator))

  def mmmProofGen: Gen[Proofs.Consensus.MMM] =
    for {
      sigi   <- genSizedStrictBytes[Lengths.`704`.type]().map(_.data)
      sigm   <- genSizedStrictBytes[Lengths.`704`.type]().map(_.data)
      pki    <- genSizedStrictBytes[Lengths.`32`.type]().map(_.data)
      offset <- posNum[Long]
      pkl    <- genSizedStrictBytes[Lengths.`32`.type]().map(_.data)
    } yield Proofs.Consensus.MMM(sigi, sigm, pki, offset, pkl)

  def kesProofGen: Gen[Proofs.Consensus.KesCertificate] =
    for {
      sig       <- genSizedStrictBytes[Proofs.Consensus.KesCertificate.SignatureLength]()
      publicKey <- genSizedStrictBytes[Proofs.Consensus.KesCertificate.ExtendedPublicKeyLength]()
      chainCode <- genSizedStrictBytes[Proofs.Consensus.KesCertificate.ChainCodeLength]()
    } yield Proofs.Consensus.KesCertificate(sig, publicKey, chainCode)

  def vrfCertificateGen: Gen[Vrf.Certificate] =
    for {
      publicKey  <- genSizedStrictBytes[Lengths.`32`.type]().map(VerificationKeys.Ed25519(_)).map(VerificationKeys.Vrf)
      nonceProof <- genSizedStrictBytes[Lengths.`80`.type]().map(Proofs.Consensus.Nonce(_))
      testProof  <- genSizedStrictBytes[Lengths.`80`.type]().map(Proofs.Consensus.VrfTest(_))
    } yield Vrf.Certificate(publicKey, nonceProof, testProof)

  def kesCertificateGen: Gen[OperationalCertificate] =
    for {
      publicKey <- genSizedStrictBytes[Lengths.`32`.type]().map(VerificationKeys.Kes(_, 0))
      kesProof  <- kesProofGen
      mmmProof  <- mmmProofGen
    } yield KesCertificate(publicKey, kesProof, mmmProof)

  def taktikosAddressGen: Gen[TaktikosAddress] =
    for {
      paymentVerificationKeyHash <- genSizedStrictBytes[Lengths.`32`.type]()
      stakingVerificationKey     <- genSizedStrictBytes[Lengths.`32`.type]()
      signature                  <- genSizedStrictBytes[Lengths.`64`.type]()
    } yield TaktikosAddress(paymentVerificationKeyHash, stakingVerificationKey, signature)

  def headerGen(
    parentHeaderIdGen: Gen[TypedIdentifier] =
      genSizedStrictBytes[Lengths.`32`.type]().map(sized => TypedBytes(IdentifierTypes.Block.HeaderV2, sized.data)),
    parentSlotGen:     Gen[Slot] = Gen.chooseNum(0L, 50L),
    txRootGen:         Gen[TxRoot] = genSizedStrictBytes[Lengths.`32`.type](),
    bloomFilterGen:    Gen[BloomFilter] = genSizedStrictBytes[Lengths.`256`.type](),
    timestampGen:      Gen[Timestamp] = Gen.chooseNum(0L, 50L),
    heightGen:         Gen[Long] = Gen.chooseNum(0L, 20L),
    slotGen:           Gen[Slot] = Gen.chooseNum(0L, 50L),
    vrfCertificateGen: Gen[Vrf.Certificate] = vrfCertificateGen,
    kesCertificateGen: Gen[OperationalCertificate] = kesCertificateGen,
    thresholdEvidenceGen: Gen[Evidence] = genSizedStrictBytes[Lengths.`32`.type]().map(b =>
      Sized.strict[TypedBytes, Lengths.`33`.type](TypedBytes(1: Byte, b.data)).toOption.get
    ),
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
      vrfCertificate <- vrfCertificateGen
      kesCertificate <- kesCertificateGen
      threshold      <- thresholdEvidenceGen
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
      threshold,
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
