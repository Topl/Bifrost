package co.topl.consensus

import co.topl.models.HasLength.implicits._
import co.topl.models._
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

  it should "invalidate blocks with non-forward slot" in {
    forAll(headerGen(slotGen = Gen.chooseNum(50, 100)), headerGen(slotGen = Gen.chooseNum(20, 49))) {
      case (parent, child) =>
        whenever(child.slot <= parent.slot) {
          val state = mock[ConsensusValidation.State]
          (() => state.parentBlockHeader)
            .expects()
            .once()
            .returning(parent)
          ConsensusStatefullyValidatable.validate(child, state).left.value shouldBe ConsensusValidation.Failures
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
        ConsensusStatefullyValidatable.validate(child, state).left.value shouldBe ConsensusValidation.Failures
          .NonForwardTimestamp(child.timestamp, parent.timestamp)
      }
    }
  }

  it should "invalidate blocks with parent-header mismatch" in {
    forAll(
      headerGen(
        slotGen = Gen.chooseNum(0, 50),
        timestampGen = Gen.chooseNum(0L, 50L)
      ),
      headerGen(
        slotGen = Gen.chooseNum(51, 100),
        timestampGen = Gen.chooseNum(51L, 100L)
      )
    ) { case (parent, child) =>
      whenever(child.slot > parent.slot && child.timestamp > parent.timestamp && child.parentHeaderId != parent.id) {
        val state = mock[ConsensusValidation.State]
        (() => state.parentBlockHeader)
          .expects()
          .once()
          .returning(parent)
        ConsensusStatefullyValidatable.validate(child, state).left.value shouldBe ConsensusValidation.Failures
          .ParentMismatch(child.parentHeaderId, parent.id)
      }
    }
  }

  // TODO: VRF Mismatch Test

  it should "validate valid blocks" in {
    forAll(
      headerGen(
        slotGen = Gen.chooseNum(0, 50),
        timestampGen = Gen.chooseNum(0L, 50L),
        heightGen = Gen.const(50)
      ),
      headerGen(
        slotGen = Gen.chooseNum(51, 100),
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
        ConsensusStatefullyValidatable.validate(child, state).value.header shouldBe child
      }
    }
  }

  def vrfCertificateGen: Gen[VrfCertificate] =
    for {
      publicKey <- Gen
        .containerOfN[Array, Byte](32, Gen.choose[Byte](0, 32))
        .map(Bytes(_))
        .map(Sized.strict[Bytes, Lengths.`32`.type](_).map(PublicKeys.Ed25519(_)).value)
      proof <- Gen
        .containerOfN[Array, Byte](64, Gen.choose[Byte](0, 32))
        .map(Bytes(_))
        .map(Sized.strict[Bytes, Lengths.`64`.type](_).value)
      testProof <- Gen
        .containerOfN[Array, Byte](80, Gen.choose[Byte](0, 32))
        .map(Bytes(_))
        .map(Sized.strict[Bytes, Lengths.`80`.type](_).value)
    } yield VrfCertificate(publicKey, proof, testProof)

  def headerGen(
    parentHeaderIdGen: Gen[TypedIdentifier] = Gen.oneOf(headerIds),
    blockBodyIdGen: Gen[TypedIdentifier] = Gen
      .containerOfN[Array, Byte](32, Gen.choose[Byte](0, 32))
      .map(Bytes(_))
      .map(TypedBytes(IdentifierTypes.Block.BodyV2, _)),
    timestampGen:      Gen[Timestamp] = Gen.chooseNum(0L, 500L),
    heightGen:         Gen[Long] = Gen.chooseNum(0L, 20L),
    slotGen:           Gen[Slot] = Gen.chooseNum(0, 50),
    vrfCertificateGen: Gen[VrfCertificate] = vrfCertificateGen,
    kesCertificateGen: Gen[KesCertificate] = Gen.containerOfN[Array, Byte](32, Gen.choose[Byte](0, 32)).map(Bytes(_))
  ): Gen[BlockHeaderV2] =
    for {
      parentHeaderID <- parentHeaderIdGen
      blockBodyId    <- blockBodyIdGen
      timestamp      <- timestampGen
      height         <- heightGen
      slot           <- slotGen
      vrfCertificate <- vrfCertificateGen
      kesCertificate <- kesCertificateGen
    } yield BlockHeaderV2(parentHeaderID, blockBodyId, timestamp, height, slot, vrfCertificate, kesCertificate)

}
