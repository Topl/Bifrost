package co.topl.models.generators.consensus

import cats.data.{Chain, NonEmptyChain}
import co.topl.consensus.models._
import co.topl.models.generators.common.ModelGenerators._
import co.topl.models.utility._
import com.google.protobuf.ByteString
import org.scalacheck.{Arbitrary, Gen}

import scala.annotation.tailrec

trait ModelGenerators {

  def thresholdEvidenceGen: Gen[Sized.Strict[ByteString, Lengths.`32`.type]] =
    genSizedStrictByteString[Lengths.`32`.type]()

  def etaGen: Gen[Sized.Strict[ByteString, Lengths.`32`.type]] =
    genSizedStrictByteString[Lengths.`32`.type]()

  def rhoGen: Gen[Sized.Strict[ByteString, Lengths.`64`.type]] =
    genSizedStrictByteString[Lengths.`64`.type]()

  // Signatures
  def signatureVrfEd25519Gen: Gen[SignatureVrfEd25519] =
    genSizedStrictByteString[Lengths.`80`.type]().map(s => SignatureVrfEd25519.of(s.data))

  def witnessGen: Gen[Sized.Strict[ByteString, Lengths.`32`.type]] =
    genSizedStrictByteString[Lengths.`32`.type]()

  def verificationKeyEd25519Gen: Gen[VerificationKeyEd25519] =
    genSizedStrictByteString[Lengths.`32`.type]().map(s => VerificationKeyEd25519.of(s.data))

  def secretKeyEd25519Gen: Gen[SecretKeyEd25519] =
    genSizedStrictByteString[Lengths.`32`.type]().map(s => SecretKeyEd25519.of(s.data))

  def signatureEd25519Gen: Gen[SignatureEd25519] =
    genSizedStrictByteString[Lengths.`64`.type]().map(s => SignatureEd25519.of(s.data))

  implicit val signatureKesSumArbitrary: Arbitrary[SignatureKesSum] =
    Arbitrary(
      for {
        verificationKey <- verificationKeyEd25519Gen
        signature       <- signatureEd25519Gen
        witness         <- Gen.nonEmptyContainerOf[Seq, Sized.Strict[ByteString, Lengths.`32`.type]](witnessGen)
      } yield SignatureKesSum.of(
        verificationKey,
        signature,
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
        superSignature,
        subSignature,
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
        signatureVrfEd25519,
        verificationKeyVrfEd25519,
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
        parentVk,
        parentSignature,
        childVK,
        childSignature
      )
    )

  implicit val arbitraryBlockId: Arbitrary[BlockId] =
    Arbitrary(
      for {
        value <- genSizedStrictByteString[Lengths.`32`.type]()
      } yield BlockId.of(value.data)
    )

  def headerGen(
    parentHeaderIdGen:         Gen[BlockId] = arbitraryBlockId.arbitrary,
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

  implicit val arbitraryHeader: Arbitrary[BlockHeader] = Arbitrary(headerGen())

  implicit val arbitrarySlotId: Arbitrary[SlotId] =
    Arbitrary(
      for {
        slot    <- Gen.posNum[Long]
        blockId <- arbitraryBlockId.arbitrary
      } yield SlotId.of(slot, blockId)
    )

  implicit val arbitrarySlotData: Arbitrary[SlotData] =
    Arbitrary(
      for {
        slotId       <- arbitrarySlotId.arbitrary
        parentSlotId <- arbitrarySlotId.arbitrary
        rho          <- rhoGen
        eta          <- etaGen
        height       <- Gen.posNum[Long]
      } yield SlotData.of(slotId, parentSlotId, rho.data, eta.data, height)
    )

  @tailrec
  private def addSlotDataToChain(
    slotData: NonEmptyChain[SlotData],
    gen:      Gen[SlotData],
    count:    Long
  ): NonEmptyChain[SlotData] =
    count match {
      case 0 => slotData
      case _ =>
        addSlotDataToChain(slotData.append(gen.sample.get.copy(parentSlotId = slotData.last.slotId)), gen, count - 1)
    }

  implicit val arbitrarySlotDataChain: Arbitrary[NonEmptyChain[SlotData]] =
    Arbitrary(
      for {
        size <- Gen.posNum[Long]
        root <- arbitrarySlotData.arbitrary
      } yield addSlotDataToChain(NonEmptyChain.one(root), arbitrarySlotData.arbitrary, size)
    )

  implicit def chainArbOf[T](implicit a: Arbitrary[T]): Arbitrary[Chain[T]] =
    Arbitrary(Gen.listOf[T](a.arbitrary).map(Chain.apply))

  implicit def nonEmptyChainArbOf[T](implicit a: Arbitrary[T]): Arbitrary[NonEmptyChain[T]] =
    Arbitrary(Gen.nonEmptyListOf[T](a.arbitrary).map(NonEmptyChain.fromSeq(_).get))
}
object ModelGenerators extends ModelGenerators
