package co.topl.consensus

import co.topl.models._
import co.topl.models.utility.HasLength.implicits._
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Length, Lengths, Ratio, Sized}
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._
import co.topl.typeclasses.IdentifierTypes
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.nio.charset.StandardCharsets

class ConsensusStatefullyValidatableSpec
    extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with MockFactory
    with EitherValues {

  behavior of "Consensus Statefully Validatable"

  private val headerIds =
    Set.tabulate(10)(i => TypedBytes(IdentifierTypes.Block.HeaderV2, Bytes(Array(i.toByte))))

  implicit val leaderElectionConfig: LeaderElection.Config =
    LeaderElection
      .Config(lddCutoff = 0, precision = 16, baselineDifficulty = Ratio(1, 15), amplitude = Ratio(2, 5))
  it should "invalidate blocks with non-forward slot" in {
    forAll(headerGen(slotGen = Gen.chooseNum(50L, 100L)), headerGen(slotGen = Gen.chooseNum[Long](20, 49))) {
      case (parent, child) =>
        whenever(child.slot <= parent.slot) {
          val state = mock[ConsensusValidation.State]
          (() => state.parentBlockHeader)
            .expects()
            .once()
            .returning(parent)
          new ConsensusStatefullyValidatable().validate(child, state).left.value shouldBe ConsensusValidation.Failures
            .NonForwardSlot(child.slot, parent.slot)
        }
    }
  }

  it should "invalidate blocks with non-forward timestamp" in {
    forAll(headerGen(), headerGen()) { case (parent, child) =>
      whenever(child.slot > parent.slot && child.timestamp <= parent.timestamp) {
        val state = mock[ConsensusValidation.State]
        (() => state.parentBlockHeader)
          .expects()
          .once()
          .returning(parent)
        new ConsensusStatefullyValidatable().validate(child, state).left.value shouldBe ConsensusValidation.Failures
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
        val state = mock[ConsensusValidation.State]
        (() => state.parentBlockHeader)
          .expects()
          .once()
          .returning(parent)
        new ConsensusStatefullyValidatable().validate(child, state).left.value shouldBe ConsensusValidation.Failures
          .ParentMismatch(child.parentHeaderId, parent.id)
      }
    }
  }

  // TODO: VRF Mismatch Test

  it should "validate valid blocks" in {
    forAll(
      headerGen(
        slotGen = Gen.chooseNum(0L, 50L),
        timestampGen = Gen.chooseNum(0L, 50L),
        heightGen = Gen.const(50)
      ),
      headerGen(
        slotGen = Gen.chooseNum(51L, 100L),
        timestampGen = Gen.chooseNum(51L, 100L),
        heightGen = Gen.const(51),
        parentHeaderIdGen =
          Gen.const(TypedBytes(IdentifierTypes.Block.HeaderV2 +: Bytes("header50".getBytes(StandardCharsets.UTF_8))))
      )
    ) { case (parent, child) =>
      whenever(child.slot > parent.slot && child.timestamp > parent.timestamp && child.parentHeaderId == parent.id) {
        val state = mock[ConsensusValidation.State]
        (() => state.parentBlockHeader)
          .expects()
          .once()
          .returning(parent)

        (() => state.epochNonce)
          .expects()
          .once()
          .returning(Bytes(Array[Byte](1, 2, 3, 4)))
        new ConsensusStatefullyValidatable().validate(child, state).value.header shouldBe child
      }
    }
  }

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
    parentHeaderIdGen:    Gen[TypedIdentifier] = Gen.oneOf(headerIds),
    txRootGen:            Gen[TxRoot] = genSizedStrictBytes[Lengths.`32`.type](),
    bloomFilterGen:       Gen[BloomFilter] = genSizedStrictBytes[Lengths.`256`.type](),
    timestampGen:         Gen[Timestamp] = Gen.chooseNum(0L, 500L),
    heightGen:            Gen[Long] = Gen.chooseNum(0L, 20L),
    slotGen:              Gen[Slot] = Gen.chooseNum(0L, 50L),
    vrfCertificateGen:    Gen[VrfCertificate] = vrfCertificateGen,
    kesCertificateGen:    Gen[KesCertificate] = kesCertificateGen,
    thresholdEvidenceGen: Gen[Sized.Strict[Bytes, Lengths.`32`.type]] = genSizedStrictBytes[Lengths.`32`.type](),
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
