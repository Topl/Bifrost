package co.topl.consensus

import cats.Id
import cats.data.OptionT
import co.topl.consensus.ConsensusValidation.implicits._
import co.topl.consensus.crypto.Vrf
import co.topl.models._
import co.topl.models.utility.HasLength.implicits._
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Length, Lengths, Ratio, Sized}
import co.topl.typeclasses.ContainsEvidence.Instances._
import co.topl.typeclasses.ContainsEvidence.ops._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._
import co.topl.typeclasses.IdentifierTypes
import co.topl.typeclasses.crypto.KeyInitializer
import co.topl.typeclasses.crypto.KeyInitializer.Instances._
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ConsensusValidationSpec
    extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with MockFactory
    with EitherValues {

  behavior of "ConsensusValidation"

  private val headerIds =
    Set.tabulate(10)(i => TypedBytes(IdentifierTypes.Block.HeaderV2, Bytes(Array(i.toByte))))

  implicit val leaderElectionConfig: LeaderElection.Config =
    LeaderElection
      .Config(lddCutoff = 0, precision = 16, baselineDifficulty = Ratio(1, 15), amplitude = Ratio(2, 5))

  implicit val vrf: Vrf = new Vrf

  it should "invalidate blocks with non-forward slot" in {
    forAll(headerGen(slotGen = Gen.chooseNum(50L, 100L)), headerGen(slotGen = Gen.chooseNum[Long](20, 49))) {
      case (parent, child) =>
        whenever(child.slot <= parent.slot) {
          implicit val interpreter: ConsensusValidation.Algebra[Id] = mock[ConsensusValidation.Algebra[Id]]
          (() => interpreter.parentBlockHeader)
            .expects()
            .once()
            .returning(parent)

          child.validatedUsing(interpreter).value.left.value shouldBe ConsensusValidation.Failures
            .NonForwardSlot(child.slot, parent.slot)
        }
    }
  }

  it should "invalidate blocks with non-forward timestamp" in {
    forAll(headerGen(), headerGen()) { case (parent, child) =>
      whenever(child.slot > parent.slot && child.timestamp <= parent.timestamp) {
        implicit val interpreter: ConsensusValidation.Algebra[Id] = mock[ConsensusValidation.Algebra[Id]]
        (() => interpreter.parentBlockHeader)
          .expects()
          .once()
          .returning(parent)
        child.validatedUsing(interpreter).value.left.value shouldBe ConsensusValidation.Failures
          .NonForwardTimestamp(child.timestamp, parent.timestamp)
      }
    }
  }

  it should "invalidate blocks with parent-header mismatch" in {
    forAll(
      headerGen(
        slotGen = Gen.chooseNum(0L, 50L),
        timestampGen = Gen.chooseNum(0L, 50L)
      ),
      headerGen(
        slotGen = Gen.chooseNum(51L, 100L),
        timestampGen = Gen.chooseNum(51L, 100L)
      )
    ) { case (parent, child) =>
      whenever(child.slot > parent.slot && child.timestamp > parent.timestamp && child.parentHeaderId != parent.id) {
        implicit val interpreter: ConsensusValidation.Algebra[Id] = mock[ConsensusValidation.Algebra[Id]]
        (() => interpreter.parentBlockHeader)
          .expects()
          .once()
          .returning(parent)
        child.validatedUsing(interpreter).value.left.value shouldBe ConsensusValidation.Failures
          .ParentMismatch(child.parentHeaderId, parent.id)
      }
    }
  }

  // TODO: VRF Mismatch Test

  it should "validate valid blocks" in {
    forAll(
      headerGen(),
      kesCertificateGen,
      genSizedStrictBytes[Lengths.`32`.type]().flatMap(txRoot =>
        genSizedStrictBytes[Lengths.`256`.type]()
          .flatMap(bloomFilter => epochNonceGen.map(nonce => (txRoot, bloomFilter, nonce)))
      ),
      relativeStakeGen,
      vrfSecretGen,
      taktikosAddressGen
    ) { case (parent, kesCertificate, (txRoot, bloomFilter, epochNonce), relativeStake, vrfSecret, address) =>
      implicit val interpreter: ConsensusValidation.Algebra[Id] = mock[ConsensusValidation.Algebra[Id]]
      val hit = LeaderElection.hits(vrfSecret, relativeStake, parent.slot + 1, epochNonce).head
      val child =
        BlockHeaderV2(
          parentHeaderId = parent.id,
          txRoot = txRoot,
          bloomFilter = bloomFilter,
          timestamp = System.currentTimeMillis(),
          height = parent.height + 1,
          slot = hit.slot,
          vrfCertificate = hit.cert,
          kesCertificate = kesCertificate,
          thresholdEvidence = hit.threshold.evidence,
          metadata = None,
          address = address
        )

      (() => interpreter.parentBlockHeader)
        .expects()
        .anyNumberOfTimes()
        .returning(parent)

      (() => interpreter.epochNonce)
        .expects()
        .once()
        .returning(Bytes(Array[Byte](1, 2, 3, 4)))

      (interpreter
        .relativeStakeFor(_: Evidence))
        .expects(*)
        .once()
        .returning(OptionT.pure[Id](relativeStake))

      child.validatedUsing(interpreter).value.value.header shouldBe child
    }
  }

  def epochNonceGen: Gen[Nonce] =
    Gen.long.map(BigInt(_).toByteArray).map(Bytes(_))

  def relativeStakeGen: Gen[Ratio] =
    Gen.chooseNum(1L, 10L).flatMap(denominator => Ratio(1L, denominator))

  def vrfSecretGen: Gen[KeyPairs.Vrf] =
    Gen.const(KeyInitializer[KeyPairs.Vrf].random())

  def vrfCertificateGen: Gen[VrfCertificate] =
    for {
      publicKey  <- genSizedStrictBytes[Lengths.`32`.type]().map(PublicKeys.Ed25519(_)).map(PublicKeys.Vrf)
      nonceProof <- genSizedStrictBytes[Lengths.`80`.type]().map(Proofs.Consensus.Nonce)
      testProof  <- genSizedStrictBytes[Lengths.`80`.type]().map(Proofs.Consensus.VrfTest)
    } yield VrfCertificate(publicKey, nonceProof, testProof)

  def kesCertificateGen: Gen[KesCertificate] =
    for {
      publicKey     <- genSizedStrictBytes[Lengths.`32`.type]().map(PublicKeys.Kes(_, 0))
      kesProof      <- genSizedStrictBytes[Lengths.`64`.type]().map(Proofs.Consensus.KesCertificate)
      mmmProof      <- genSizedStrictBytes[Lengths.`1440`.type]().map(Proofs.Consensus.MMM)
      slotOffsetGen <- Gen.chooseNum[Long](1, 1000)
    } yield KesCertificate(publicKey, kesProof, mmmProof, slotOffsetGen)

  def taktikosAddressGen: Gen[TaktikosAddress] =
    for {
      paymentVerificationKeyHash <- genSizedStrictBytes[Lengths.`32`.type]()
      stakingVerificationKey     <- genSizedStrictBytes[Lengths.`32`.type]()
      signature                  <- genSizedStrictBytes[Lengths.`64`.type]()
    } yield TaktikosAddress(paymentVerificationKeyHash, stakingVerificationKey, signature)

  def headerGen(
    parentHeaderIdGen: Gen[TypedIdentifier] = Gen.oneOf(headerIds),
    txRootGen:         Gen[TxRoot] = genSizedStrictBytes[Lengths.`32`.type](),
    bloomFilterGen:    Gen[BloomFilter] = genSizedStrictBytes[Lengths.`256`.type](),
    timestampGen:      Gen[Timestamp] = Gen.chooseNum(0L, 500L),
    heightGen:         Gen[Long] = Gen.chooseNum(0L, 20L),
    slotGen:           Gen[Slot] = Gen.chooseNum(0L, 50L),
    vrfCertificateGen: Gen[VrfCertificate] = vrfCertificateGen,
    kesCertificateGen: Gen[KesCertificate] = kesCertificateGen,
    thresholdEvidenceGen: Gen[Evidence] = genSizedStrictBytes[Lengths.`32`.type]().map(b =>
      Sized.strict[TypedBytes, Lengths.`33`.type](TypedBytes(1: Byte, b.data)).value
    ),
    metadataGen: Gen[Option[Sized.Max[Latin1Data, Lengths.`32`.type]]] = Gen.option(
      Gen
        .containerOfN[Array, Byte](32, Gen.choose[Byte](0, 32))
        .map(Latin1Data(_))
        .map(Sized.max[Latin1Data, Lengths.`32`.type](_).value)
    ),
    addressGen: Gen[TaktikosAddress] = taktikosAddressGen
  ): Gen[BlockHeaderV2] =
    for {
      parentHeaderID <- parentHeaderIdGen
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
      .map(Sized.max[Bytes, L](_).value)

  def genSizedStrictBytes[L <: Length](
    byteGen:    Gen[Byte] = Gen.choose[Byte](0, 32)
  )(implicit l: L): Gen[Sized.Strict[Bytes, L]] =
    Gen
      .containerOfN[Array, Byte](l.value, byteGen)
      .map(Bytes(_))
      .map(Sized.strict[Bytes, L](_).value)

}
