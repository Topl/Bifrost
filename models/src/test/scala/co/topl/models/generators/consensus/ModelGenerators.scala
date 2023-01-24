package co.topl.models.generators.consensus

import cats.implicits.catsSyntaxOptionId
import co.topl.consensus.models._
import co.topl.models.generators.common.ModelGenerators._
import co.topl.models.generators.crypto.ModelGenerators._
import co.topl.models.utility.{Lengths, Sized}
import com.google.protobuf.ByteString
import org.scalacheck.{Arbitrary, Gen}

trait ModelGenerators {

  def thresholdEvidenceGen: Gen[Sized.Strict[ByteString, Lengths.`32`.type]] =
    genSizedStrictByteString[Lengths.`32`.type]()

  def etaGen: Gen[Sized.Strict[ByteString, Lengths.`32`.type]] =
    genSizedStrictByteString[Lengths.`32`.type]()

  // Signatures
  def signatureVrfEd25519Gen: Gen[SignatureVrfEd25519] =
    genSizedStrictByteString[Lengths.`80`.type]().map(s => SignatureVrfEd25519.of(s.data))

  def witnessGen: Gen[Sized.Strict[ByteString, Lengths.`32`.type]] =
    genSizedStrictByteString[Lengths.`32`.type]()

  implicit val signatureKesSumArbitrary: Arbitrary[SignatureKesSum] =
    Arbitrary(
      for {
        verificationKey <- verificationKeyEd25519Gen
        signature       <- signatureEd25519Gen
        witness         <- Gen.nonEmptyContainerOf[Seq, Sized.Strict[ByteString, Lengths.`32`.type]](witnessGen)
      } yield SignatureKesSum.of(
        verificationKey.some,
        signature.some,
        witness.map(_.data)
      )
    )

  implicit val signatureKesProductArbitrary: Arbitrary[SignatureKesProduct] =
    Arbitrary(
      for {
        superSignature <- signatureKesSumArbitrary.arbitrary
        subSignature   <- signatureKesSumArbitrary.arbitrary
        subRoot        <- genSizedStrictByteString[Lengths.`32`.type]()
      } yield SignatureKesProduct.of(
        superSignature.some,
        subSignature.some,
        subRoot.data
      )
    )

  // Verifications
  def vkVrfEd25519Gen: Gen[VerificationKeyVrfEd25519] =
    genSizedStrictByteString[Lengths.`32`.type]().map(s => VerificationKeyVrfEd25519(s.data))

  implicit val arbitraryEligibilityCertificate: Arbitrary[EligibilityCertificate] =
    Arbitrary(
      for {
        signatureVrfEd25519       <- signatureVrfEd25519Gen
        verificationKeyVrfEd25519 <- vkVrfEd25519Gen
        thresholdEvidence         <- thresholdEvidenceGen
        eta                       <- etaGen
      } yield EligibilityCertificate.of(
        signatureVrfEd25519.some,
        verificationKeyVrfEd25519.some,
        thresholdEvidence.data,
        eta.data
      )
    )

  implicit val arbitraryVerificationKeyKesProduct: Arbitrary[VerificationKeyKesProduct] =
    Arbitrary(
      for {
        value <- genSizedStrictByteString[Lengths.`32`.type]()
        step  <- Gen.posNum[Int]
      } yield VerificationKeyKesProduct.of(
        value.data,
        step
      )
    )

  implicit val arbitraryOperationalCertificate: Arbitrary[OperationalCertificate] =
    Arbitrary(
      for {
        parentVk        <- arbitraryVerificationKeyKesProduct.arbitrary
        parentSignature <- signatureKesProductArbitrary.arbitrary
        childVK         <- verificationKeyEd25519Gen
        childSignature  <- signatureEd25519Gen
      } yield OperationalCertificate.of(
        parentVk.some,
        parentSignature.some,
        childVK.some,
        childSignature.some
      )
    )

  implicit val blockIdGen: Gen[BlockId] =
    genSizedStrictByteString[Lengths.`32`.type]().map(s => BlockId(s.data))

  def headerGen(
    parentHeaderIdGen:         Gen[BlockId] = blockIdGen,
    parentSlotGen:             Gen[Long] = Gen.chooseNum(0L, 50L),
    txRootGen:                 Gen[ByteString] = genSizedStrictByteString[Lengths.`32`.type]().map(_.data),
    bloomFilterGen:            Gen[ByteString] = genSizedStrictByteString[Lengths.`256`.type]().map(_.data),
    timestampGen:              Gen[Long] = Gen.chooseNum(0L, 50L),
    heightGen:                 Gen[Long] = Gen.chooseNum(0L, 20L),
    slotGen:                   Gen[Long] = Gen.chooseNum(0L, 50L),
    eligibilityCertificateGen: Gen[EligibilityCertificate] = arbitraryEligibilityCertificate.arbitrary,
    operationalCertificateGen: Gen[OperationalCertificate] = arbitraryOperationalCertificate.arbitrary,
    metadataGen:               Gen[ByteString] = genSizedStrictByteString[Lengths.`32`.type]().map(_.data),
    addressGen:                Gen[ByteString] = genSizedStrictByteString[Lengths.`32`.type]().map(_.data)
  ): Gen[BlockHeader] =
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
    } yield BlockHeader(
      Some(parentHeaderID),
      parentSlot,
      txRoot,
      bloomFilter,
      timestamp,
      height,
      slot,
      Some(vrfCertificate),
      Some(kesCertificate),
      metadata,
      address
    )

  implicit val arbitraryHeader: Arbitrary[BlockHeader] = Arbitrary(headerGen())

}
object ModelGenerators extends ModelGenerators
