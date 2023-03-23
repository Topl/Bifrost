package co.topl.consensus.interpreters

import cats.MonadThrow
import cats.Show
import cats.effect._
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.algebras.Store
import co.topl.algebras.UnsafeResource
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras._
import co.topl.consensus.models._
import co.topl.consensus.thresholdEvidence
import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.hash.Blake2b512
import co.topl.crypto.models.SecretKeyKesProduct
import co.topl.crypto.signing._
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models._
import co.topl.models.generators.common.ModelGenerators.genSizedStrictByteString
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.models.utility.HasLength.instances.byteStringLength
import co.topl.models.utility._
import co.topl.numerics.interpreters.ExpInterpreter
import co.topl.numerics.interpreters.Log1pInterpreter
import com.google.common.primitives.Longs
import com.google.protobuf.ByteString
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scodec.bits.ByteVector

import java.util.UUID
import scala.util.Random

class BlockHeaderValidationSpec
    extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with MockFactory
    with EitherValues {

  behavior of "ConsensusValidation"

  type F[A] = IO[A]

  import BlockHeaderValidationSpec._

  implicit private val ed25519Vrf: Ed25519VRF =
    Ed25519VRF.precomputed()

  implicit private val ed25519: Ed25519 =
    new Ed25519

  implicit private val kesProduct: KesProduct =
    new KesProduct

  private val blake2b256: Blake2b256 =
    new Blake2b256

  private val blake2b512: Blake2b512 =
    new Blake2b512

  private val expInterpreter = ExpInterpreter.make[F](10000, 38).unsafeRunSync()

  private val log1pInterpreter = Log1pInterpreter.make[F](10000, 16).unsafeRunSync()

  private val log1pCached = Log1pInterpreter.makeCached[F](log1pInterpreter).unsafeRunSync()

  private val bigBangBlockId = arbitraryBlockId.arbitrary.first

  private val leaderElectionInterpreter =
    LeaderElectionValidation
      .makeCached[F](
        LeaderElectionValidation.make[F](
          VrfConfig(lddCutoff = 0, precision = 16, baselineDifficulty = Ratio(1, 15), amplitude = Ratio(2, 5)),
          new UnsafeResource[F, Blake2b512] {
            def use[Res](f: Blake2b512 => F[Res]): F[Res] = f(blake2b512)
          },
          expInterpreter,
          log1pCached
        )
      )
      .unsafeRunSync()

  private def createDummyClockAlgebra(child: BlockHeader) = {
    val clock: ClockAlgebra[F] = mock[ClockAlgebra[F]]
    (() => clock.globalSlot)
      .expects()
      .once()
      .returning(child.slot.pure[F])

    (clock
      .timestampToSlot(_: Timestamp))
      .expects(*)
      .once()
      .returning(child.slot.pure[F])

    (() => clock.forwardBiasedSlotWindow)
      .expects()
      .once()
      .returning(1L.pure[F])

    clock
  }

  it should "invalidate blocks with non-forward slot" in {
    forAll(genValid(u => u.copy(slot = 0L))) { case (parent, child, _: SignatureKesProduct, _: Eta, _: Ratio) =>
      val etaInterpreter = mock[EtaCalculationAlgebra[F]]
      val consensusValidationState = mock[ConsensusValidationStateAlgebra[F]]
      val clockAlgebra = mock[ClockAlgebra[F]]
      val headerStore = simpleHeaderStore(child.parentHeaderId, parent)
      val ed25519VRFResource = mock[UnsafeResource[F, Ed25519VRF]]
      val kesProductResource = mock[UnsafeResource[F, KesProduct]]
      val ed25519Resource = mock[UnsafeResource[F, Ed25519]]
      val blake2b256Resource = mock[UnsafeResource[F, Blake2b256]]

      val underTest =
        BlockHeaderValidation
          .make[F](
            etaInterpreter,
            consensusValidationState,
            leaderElectionInterpreter,
            clockAlgebra,
            headerStore,
            bigBangBlockId,
            ed25519VRFResource,
            kesProductResource,
            ed25519Resource,
            blake2b256Resource
          )
          .unsafeRunSync()

      underTest.validate(child).unsafeRunSync().left.value shouldBe BlockHeaderValidationFailures
        .NonForwardSlot(child.slot, parent.slot)
    }
  }

  it should "invalidate blocks with non-forward timestamp" in {
    forAll(genValid(u => u.copy(timestamp = 0L))) { case (parent, child, _: SignatureKesProduct, _: Eta, _: Ratio) =>
      val etaInterpreter = mock[EtaCalculationAlgebra[F]]
      val consensusValidationState = mock[ConsensusValidationStateAlgebra[F]]
      val clockAlgebra = mock[ClockAlgebra[F]]
      val headerStore = simpleHeaderStore(child.parentHeaderId, parent)
      val ed25519VRFResource = mock[UnsafeResource[F, Ed25519VRF]]
      val kesProductResource = mock[UnsafeResource[F, KesProduct]]
      val ed25519Resource = mock[UnsafeResource[F, Ed25519]]
      val blake2b256Resource = mock[UnsafeResource[F, Blake2b256]]
      val underTest =
        BlockHeaderValidation
          .make[F](
            etaInterpreter,
            consensusValidationState,
            leaderElectionInterpreter,
            clockAlgebra,
            headerStore,
            bigBangBlockId,
            ed25519VRFResource,
            kesProductResource,
            ed25519Resource,
            blake2b256Resource
          )
          .unsafeRunSync()

      underTest.validate(child).unsafeRunSync().left.value shouldBe BlockHeaderValidationFailures
        .NonForwardTimestamp(child.timestamp, parent.timestamp)
    }
  }

  it should "invalidate blocks with incorrect timestamp for slot" in {
    val generator =
      for {
        slotError <- Gen.chooseNum[Long](Int.MinValue, Int.MaxValue).suchThat(_ != 0)
        (parent, child, _: SignatureKesProduct, _: Eta, _: Ratio) <- genValid()
      } yield (parent, child, slotError)

    forAll(generator) { case (parent, child, slotError) =>
      val etaInterpreter = mock[EtaCalculationAlgebra[F]]
      val consensusValidationState = mock[ConsensusValidationStateAlgebra[F]]
      val ed25519VRFResource = mock[UnsafeResource[F, Ed25519VRF]]
      val kesProductResource = mock[UnsafeResource[F, KesProduct]]
      val ed25519Resource = mock[UnsafeResource[F, Ed25519]]
      val blake2b256Resource = mock[UnsafeResource[F, Blake2b256]]

      val clockAlgebra = mock[ClockAlgebra[F]]
      val headerStore = simpleHeaderStore(child.parentHeaderId, parent)
      (() => clockAlgebra.globalSlot)
        .expects()
        .once()
        .returning(child.slot.pure[F])

      (clockAlgebra
        .timestampToSlot(_: Timestamp))
        .expects(child.timestamp)
        .once()
        .returning((child.slot + slotError).pure[F]) // return incorrect slot for child block

      (() => clockAlgebra.forwardBiasedSlotWindow)
        .expects()
        .once()
        .returning(1L.pure[F])

      val underTest =
        BlockHeaderValidation
          .make[F](
            etaInterpreter,
            consensusValidationState,
            leaderElectionInterpreter,
            clockAlgebra,
            headerStore,
            bigBangBlockId,
            ed25519VRFResource,
            kesProductResource,
            ed25519Resource,
            blake2b256Resource
          )
          .unsafeRunSync()

      underTest.validate(child).unsafeRunSync().left.value shouldBe BlockHeaderValidationFailures
        .TimestampSlotMismatch(child.slot, child.timestamp)
    }
  }

  // TODO do we need to care about long overflow if (forwardWindowBias + global slot) > Long.MaxValue?
  it should "invalidate block if slot is greater than current global slot + forward biased slot window" in {
    val generator =
      for {
        globalSlot        <- Gen.chooseNum[Long](1, Int.MaxValue)
        parentSlot        <- Gen.const[Long](globalSlot - 1)
        forwardSlotWindow <- Gen.chooseNum[Long](1, Int.MaxValue)
        biasValue         <- Gen.chooseNum[Long](1, Int.MaxValue)
        slotFromFuture    <- Gen.chooseNum[Long](globalSlot + forwardSlotWindow + biasValue, Long.MaxValue)
        (parent, child, _: SignatureKesProduct, _: Eta, _: Ratio) <- genValid(
          childHeader => childHeader.copy(slot = slotFromFuture),
          parentSlot = parentSlot
        )
      } yield (parent, child, forwardSlotWindow, globalSlot)

    forAll(generator) { case (parent, child, forwardWindowBias, globalSlot) =>
      val etaInterpreter = mock[EtaCalculationAlgebra[F]]
      val consensusValidationState = mock[ConsensusValidationStateAlgebra[F]]
      val ed25519VRFResource = mock[UnsafeResource[F, Ed25519VRF]]
      val kesProductResource = mock[UnsafeResource[F, KesProduct]]
      val ed25519Resource = mock[UnsafeResource[F, Ed25519]]
      val blake2b256Resource = mock[UnsafeResource[F, Blake2b256]]

      val clockAlgebra = mock[ClockAlgebra[F]]
      val headerStore = simpleHeaderStore(child.parentHeaderId, parent)
      (() => clockAlgebra.globalSlot)
        .expects()
        .once()
        .returning(globalSlot.pure[F])

      (clockAlgebra
        .timestampToSlot(_: Timestamp))
        .expects(child.timestamp)
        .once()
        .returning(child.slot.pure[F])

      (() => clockAlgebra.forwardBiasedSlotWindow)
        .expects()
        .once()
        .returning(forwardWindowBias.pure[F])

      val underTest =
        BlockHeaderValidation
          .make[F](
            etaInterpreter,
            consensusValidationState,
            leaderElectionInterpreter,
            clockAlgebra,
            headerStore,
            bigBangBlockId,
            ed25519VRFResource,
            kesProductResource,
            ed25519Resource,
            blake2b256Resource
          )
          .unsafeRunSync()

      underTest.validate(child).unsafeRunSync().left.value shouldBe BlockHeaderValidationFailures
        .SlotBeyondForwardBiasedSlotWindow(globalSlot, child.slot)
    }
  }

  it should "invalidate blocks with syntactically incorrect VRF certificate for a particular nonce" in {
    forAll(
      headerGen(
        slotGen = Gen.chooseNum(0L, 50L),
        timestampGen = Gen.chooseNum(0L, 50L),
        heightGen = Gen.const(1L)
      ).flatMap(parent =>
        // The child block has a generated VRF Certificate (generated test/nonce proofs), meaning the proofs will not
        // match the epoch nonce `[1]` used in the test body
        headerGen(
          slotGen = Gen.chooseNum(51L, 100L),
          timestampGen = Gen.chooseNum(51L, 100L),
          parentSlotGen = Gen.const(parent.slot),
          parentHeaderIdGen = Gen.const(parent.id),
          heightGen = Gen.const(2L)
        )
          .map(parent -> _)
      ),
      co.topl.models.ModelGenerators.etaGen // TODO replace with new model
    ) { case ((parent, child), eta) =>
      val etaInterpreter = mock[EtaCalculationAlgebra[F]]
      val consensusValidationState = mock[ConsensusValidationStateAlgebra[F]]
      val clockAlgebra = createDummyClockAlgebra(child)
      val headerStore = simpleHeaderStore(child.parentHeaderId, parent)
      val ed25519VRFResource = mock[UnsafeResource[F, Ed25519VRF]]
      val kesProductResource = mock[UnsafeResource[F, KesProduct]]
      val ed25519Resource = mock[UnsafeResource[F, Ed25519]]
      val blake2b256Resource = mock[UnsafeResource[F, Blake2b256]]
      val underTest =
        BlockHeaderValidation
          .make[F](
            etaInterpreter,
            consensusValidationState,
            leaderElectionInterpreter,
            clockAlgebra,
            headerStore,
            bigBangBlockId,
            ed25519VRFResource,
            kesProductResource,
            ed25519Resource,
            blake2b256Resource
          )
          .unsafeRunSync()

      (etaInterpreter
        .etaToBe(_: SlotId, _: Slot))
        .expects(SlotId(parent.slot, parent.id), child.slot)
        .anyNumberOfTimes()
        // This epoch nonce does not satisfy the generated VRF certificate
        .returning(eta.pure[F])

      (ed25519VRFResource
        .use[Boolean](_: Function1[Ed25519VRF, F[Boolean]]))
        .expects(*)
        .anyNumberOfTimes()
        .onCall { f: Function1[Ed25519VRF, F[Boolean]] => f(ed25519Vrf) }

      (ed25519VRFResource
        .use[Rho](_: Function1[Ed25519VRF, F[Rho]]))
        .expects(*)
        .anyNumberOfTimes()
        .onCall { f: Function1[Ed25519VRF, F[Rho]] => f(ed25519Vrf) }

      underTest
        .validate(child)
        .unsafeRunSync()
        .left
        .value shouldBe a[BlockHeaderValidationFailures.InvalidEligibilityCertificateEta]
    }
  }

  it should "invalidate blocks with a syntactically incorrect KES certificate" in {
    forAll(genValid()) { case (parent, child, _: SignatureKesProduct, eta, _: Ratio) =>
      val etaInterpreter = mock[EtaCalculationAlgebra[F]]
      val consensusValidationState = mock[ConsensusValidationStateAlgebra[F]]
      val headerStore = simpleHeaderStore(child.parentHeaderId, parent)
      val clockAlgebra = createDummyClockAlgebra(child)
      val ed25519VRFResource = mock[UnsafeResource[F, Ed25519VRF]]
      val kesProductResource = mock[UnsafeResource[F, KesProduct]]
      val ed25519Resource = mock[UnsafeResource[F, Ed25519]]
      val blake2b256Resource = mock[UnsafeResource[F, Blake2b256]]
      val underTest =
        BlockHeaderValidation
          .make[F](
            etaInterpreter,
            consensusValidationState,
            leaderElectionInterpreter,
            clockAlgebra,
            headerStore,
            bigBangBlockId,
            ed25519VRFResource,
            kesProductResource,
            ed25519Resource,
            blake2b256Resource
          )
          .unsafeRunSync()

      // Changing any bytes of the block will result in a bad block signature
      val badBlock = child.copy(timestamp = child.timestamp + 1)

      (etaInterpreter
        .etaToBe(_: SlotId, _: Slot))
        .expects(SlotId(parent.slot, parent.id), badBlock.slot)
        .anyNumberOfTimes()
        .returning(eta.pure[F])

      (ed25519VRFResource
        .use[Boolean](_: Function1[Ed25519VRF, F[Boolean]]))
        .expects(*)
        .anyNumberOfTimes()
        .onCall { f: Function1[Ed25519VRF, F[Boolean]] => f(ed25519Vrf) }

      (ed25519VRFResource
        .use[Rho](_: Function1[Ed25519VRF, F[Rho]]))
        .expects(*)
        .anyNumberOfTimes()
        .onCall { f: Function1[Ed25519VRF, F[Rho]] => f(ed25519Vrf) }

      (kesProductResource
        .use[Boolean](_: Function1[KesProduct, F[Boolean]]))
        .expects(*)
        .anyNumberOfTimes()
        .onCall { f: Function1[KesProduct, F[Boolean]] => f(kesProduct) }

      (ed25519Resource
        .use[Boolean](_: Function1[Ed25519, F[Boolean]]))
        .expects(*)
        .anyNumberOfTimes()
        .onCall { f: Function1[Ed25519, F[Boolean]] => f(ed25519) }

      underTest
        .validate(badBlock)
        .unsafeRunSync()
        .left
        .value shouldBe a[BlockHeaderValidationFailures.InvalidBlockProof]
    }
  }

  it should "invalidate blocks with a semantically incorrect registration verification" in {
    forAll(
      genValid(u =>
        u.copy(
          address = StakingAddress(ByteString.copyFrom(Array.fill[Byte](32)(0)))
        )
      )
    ) { case (parent, child, registration, eta, _: Ratio) =>
      val etaInterpreter = mock[EtaCalculationAlgebra[F]]
      val consensusValidationState = mock[ConsensusValidationStateAlgebra[F]]
      val headerStore = simpleHeaderStore(child.parentHeaderId, parent)
      val clockAlgebra = createDummyClockAlgebra(child)
      val ed25519VRFResource = mock[UnsafeResource[F, Ed25519VRF]]
      val kesProductResource = mock[UnsafeResource[F, KesProduct]]
      val ed25519Resource = mock[UnsafeResource[F, Ed25519]]
      val blake2b256Resource = mock[UnsafeResource[F, Blake2b256]]
      val underTest =
        BlockHeaderValidation
          .make[F](
            etaInterpreter,
            consensusValidationState,
            leaderElectionInterpreter,
            clockAlgebra,
            headerStore,
            bigBangBlockId,
            ed25519VRFResource,
            kesProductResource,
            ed25519Resource,
            blake2b256Resource
          )
          .unsafeRunSync()

      (consensusValidationState
        .operatorRegistration(_: BlockId, _: Slot)(_: StakingAddress))
        .expects(*, *, *)
        .once()
        .returning(registration.some.pure[F])

      (etaInterpreter
        .etaToBe(_: SlotId, _: Slot))
        .expects(SlotId(parent.slot, parent.id), child.slot)
        .anyNumberOfTimes()
        .returning(eta.pure[F])

      (ed25519VRFResource
        .use[Boolean](_: Function1[Ed25519VRF, F[Boolean]]))
        .expects(*)
        .anyNumberOfTimes()
        .onCall { f: Function1[Ed25519VRF, F[Boolean]] => f(ed25519Vrf) }

      (ed25519VRFResource
        .use[Rho](_: Function1[Ed25519VRF, F[Rho]]))
        .expects(*)
        .anyNumberOfTimes()
        .onCall { f: Function1[Ed25519VRF, F[Rho]] => f(ed25519Vrf) }

      (kesProductResource
        .use[Boolean](_: Function1[KesProduct, F[Boolean]]))
        .expects(*)
        .anyNumberOfTimes()
        .onCall { f: Function1[KesProduct, F[Boolean]] => f(kesProduct) }

      (ed25519Resource
        .use[Boolean](_: Function1[Ed25519, F[Boolean]]))
        .expects(*)
        .anyNumberOfTimes()
        .onCall { f: Function1[Ed25519, F[Boolean]] => f(ed25519) }

      (blake2b256Resource
        .use[ByteVector](_: Function1[Blake2b256, F[ByteVector]]))
        .expects(*)
        .anyNumberOfTimes()
        .onCall { f: Function1[Blake2b256, F[ByteVector]] => f(blake2b256) }

      underTest
        .validate(child)
        .unsafeRunSync()
        .left
        .value shouldBe a[BlockHeaderValidationFailures.RegistrationCommitmentMismatch]
    }
  }

  it should "invalidate blocks with an insufficient VRF threshold" in {
    forAll(genValid()) { case (parent, child, registration, eta, _: Ratio) =>
      val etaInterpreter = mock[EtaCalculationAlgebra[F]]
      val consensusValidationState = mock[ConsensusValidationStateAlgebra[F]]
      val headerStore = simpleHeaderStore(child.parentHeaderId, parent)
      val clockAlgebra = createDummyClockAlgebra(child)
      val ed25519VRFResource = mock[UnsafeResource[F, Ed25519VRF]]
      val kesProductResource = mock[UnsafeResource[F, KesProduct]]
      val ed25519Resource = mock[UnsafeResource[F, Ed25519]]
      val blake2b256Resource = mock[UnsafeResource[F, Blake2b256]]
      val underTest =
        BlockHeaderValidation
          .make[F](
            etaInterpreter,
            consensusValidationState,
            leaderElectionInterpreter,
            clockAlgebra,
            headerStore,
            bigBangBlockId,
            ed25519VRFResource,
            kesProductResource,
            ed25519Resource,
            blake2b256Resource
          )
          .unsafeRunSync()

      (consensusValidationState
        .operatorRegistration(_: BlockId, _: Slot)(_: StakingAddress))
        .expects(*, *, *)
        .once()
        .returning(registration.some.pure[F])

      (etaInterpreter
        .etaToBe(_: SlotId, _: Slot))
        .expects(SlotId(parent.slot, parent.id), child.slot)
        .anyNumberOfTimes()
        .returning(eta.pure[F])

      (consensusValidationState
        .operatorRelativeStake(_: BlockId, _: Slot)(_: StakingAddress))
        .expects(*, *, *)
        .once()
        .returning(Ratio.Zero.some.pure[F])

      (ed25519VRFResource
        .use[Boolean](_: Function1[Ed25519VRF, F[Boolean]]))
        .expects(*)
        .anyNumberOfTimes()
        .onCall { f: Function1[Ed25519VRF, F[Boolean]] => f(ed25519Vrf) }

      (ed25519VRFResource
        .use[Rho](_: Function1[Ed25519VRF, F[Rho]]))
        .expects(*)
        .anyNumberOfTimes()
        .onCall { f: Function1[Ed25519VRF, F[Rho]] => f(ed25519Vrf) }

      (kesProductResource
        .use[Boolean](_: Function1[KesProduct, F[Boolean]]))
        .expects(*)
        .anyNumberOfTimes()
        .onCall { f: Function1[KesProduct, F[Boolean]] => f(kesProduct) }

      (ed25519Resource
        .use[Boolean](_: Function1[Ed25519, F[Boolean]]))
        .expects(*)
        .anyNumberOfTimes()
        .onCall { f: Function1[Ed25519, F[Boolean]] => f(ed25519) }

      (blake2b256Resource
        .use[ByteVector](_: Function1[Blake2b256, F[ByteVector]]))
        .expects(*)
        .anyNumberOfTimes()
        .onCall { f: Function1[Blake2b256, F[ByteVector]] => f(blake2b256) }

      underTest
        .validate(child)
        .unsafeRunSync()
        .left
        .value shouldBe a[BlockHeaderValidationFailures.InvalidVrfThreshold]
    }
  }

  // TODO: Fix this test, talk with Sean
  it should "validate valid blocks" in {
    forAll(genValid()) { case (parent, child, registration, eta, relativeStake) =>
      val etaInterpreter = mock[EtaCalculationAlgebra[F]]
      val consensusValidationState = mock[ConsensusValidationStateAlgebra[F]]
      val headerStore = simpleHeaderStore(child.parentHeaderId, parent)
      val clockAlgebra = createDummyClockAlgebra(child)
      val ed25519VRFResource = mock[UnsafeResource[F, Ed25519VRF]]
      val kesProductResource = mock[UnsafeResource[F, KesProduct]]
      val ed25519Resource = mock[UnsafeResource[F, Ed25519]]
      val blake2b256Resource = mock[UnsafeResource[F, Blake2b256]]
      val underTest =
        BlockHeaderValidation
          .make[F](
            etaInterpreter,
            consensusValidationState,
            leaderElectionInterpreter,
            clockAlgebra,
            headerStore,
            bigBangBlockId,
            ed25519VRFResource,
            kesProductResource,
            ed25519Resource,
            blake2b256Resource
          )
          .unsafeRunSync()

      (consensusValidationState
        .operatorRegistration(_: BlockId, _: Slot)(_: StakingAddress))
        .expects(*, *, *)
        .once()
        .returning(registration.some.pure[F])

      (etaInterpreter
        .etaToBe(_: SlotId, _: Slot))
        .expects(SlotId(parent.slot, parent.id), child.slot)
        .anyNumberOfTimes()
        .returning(eta.pure[F])

      (consensusValidationState
        .operatorRelativeStake(_: BlockId, _: Slot)(_: StakingAddress))
        .expects(*, *, *)
        .once()
        .returning(relativeStake.some.pure[F])

      (ed25519VRFResource
        .use[Boolean](_: Function1[Ed25519VRF, F[Boolean]]))
        .expects(*)
        .anyNumberOfTimes()
        .onCall { f: Function1[Ed25519VRF, F[Boolean]] => f(ed25519Vrf) }

      (ed25519VRFResource
        .use[Rho](_: Function1[Ed25519VRF, F[Rho]]))
        .expects(*)
        .anyNumberOfTimes()
        .onCall { f: Function1[Ed25519VRF, F[Rho]] => f(ed25519Vrf) }

      (kesProductResource
        .use[Boolean](_: Function1[KesProduct, F[Boolean]]))
        .expects(*)
        .anyNumberOfTimes()
        .onCall { f: Function1[KesProduct, F[Boolean]] => f(kesProduct) }

      (ed25519Resource
        .use[Boolean](_: Function1[Ed25519, F[Boolean]]))
        .expects(*)
        .anyNumberOfTimes()
        .onCall { f: Function1[Ed25519, F[Boolean]] => f(ed25519) }

      (blake2b256Resource
        .use[ByteVector](_: Function1[Blake2b256, F[ByteVector]]))
        .expects(*)
        .anyNumberOfTimes()
        .onCall { f: Function1[Blake2b256, F[ByteVector]] => f(blake2b256) }

      underTest.validate(child).unsafeRunSync().value shouldBe child
    }
  }

  private def validEligibilityCertificate(
    skVrf:                ByteString,
    thresholdInterpreter: LeaderElectionValidationAlgebra[F],
    eta:                  Eta,
    relativeStake:        Ratio,
    parentSlot:           Slot
  ): (co.topl.consensus.models.EligibilityCertificate, Slot) = {
    def proof(slot: Slot) =
      ByteString.copyFrom(
        ed25519Vrf.sign(skVrf.toByteArray, VrfArgument(eta, slot).signableBytes.toByteArray)
      )

    def isLeader(threshold: Ratio, testProof: ByteString) =
      thresholdInterpreter
        .isSlotLeaderForThreshold(threshold)(
          Rho(Sized.strictUnsafe(ByteString.copyFrom(ed25519Vrf.proofToHash(testProof.toByteArray))))
        )
        .unsafeRunSync()

    var slot = parentSlot + 1
    var testProof = proof(slot)
    var threshold = thresholdInterpreter.getThreshold(relativeStake, slot).unsafeRunSync()
    while (!isLeader(threshold, testProof)) {
      slot += 1
      testProof = proof(slot)
      threshold = thresholdInterpreter.getThreshold(relativeStake, slot).unsafeRunSync()
    }
    val cert = co.topl.consensus.models.EligibilityCertificate(
      testProof,
      ByteString.copyFrom(ed25519Vrf.getVerificationKey(skVrf.toByteArray)),
      thresholdEvidence(threshold)(new Blake2b256),
      ByteString.copyFrom(eta.data.toArray)
    )

    cert -> slot
  }

  private def withPartialOperationalCertificate(
    slot:      Slot,
    unsignedF: UnsignedBlockHeader.PartialOperationalCertificate => UnsignedBlockHeader,
    parentSK:  SecretKeyKesProduct
  ): (UnsignedBlockHeader, ByteString) = {
    val keyPair =
      new Ed25519().deriveKeyPairFromEntropy(Entropy.fromUuid(UUID.randomUUID()), None)
    val linearSKBytes = keyPair.signingKey.bytes
    val linearVKBytes = keyPair.verificationKey.bytes
    val message = linearVKBytes ++ Longs.toByteArray(slot)
    val parentSignature = kesProduct.sign(parentSK, message)
    val kesProductVerificationKey = kesProduct.getVerificationKey(parentSK)
    val partialCertificate = UnsignedBlockHeader.PartialOperationalCertificate(
      kesProductVerificationKey,
      parentSignature,
      ByteString.copyFrom(linearVKBytes)
    )
    unsignedF(partialCertificate) -> ByteString.copyFrom(linearSKBytes)
  }

  private def genValid(
    preSign:    UnsignedBlockHeader => UnsignedBlockHeader = identity,
    parentSlot: Slot = 5000L
  ): Gen[(BlockHeader, BlockHeader, SignatureKesProduct, Eta, Ratio)] =
    for {
      parent      <- headerGen(slotGen = Gen.const[Long](parentSlot))
      txRoot      <- genSizedStrictByteString[Lengths.`32`.type]()
      bloomFilter <- genSizedStrictByteString[Lengths.`256`.type]()
      eta <- co.topl.models.ModelGenerators.etaGen // TODO replace model when validEligibilityCertificate is replaced
      relativeStake       <- co.topl.models.ModelGenerators.relativeStakeGen
      (vrfSecretBytes, _) <- Gen.const(Ed25519VRF.precomputed().generateRandom)
    } yield {
      val (kesSK0, _) = kesProduct.createKeyPair(Random.nextBytes(32), (9, 9), 0L)
      val poolVK = ByteString.copyFrom(Random.nextBytes(32))
      val stakingAddress = StakingAddress(poolVK)
      val vrfSecret = ByteString.copyFrom(vrfSecretBytes)

      val registration = validRegistrationNew(
        ByteString.copyFrom(ed25519Vrf.getVerificationKey(vrfSecretBytes)),
        stakingAddress,
        kesSK0
      )

      val (eligibilityCertificate, slot) =
        validEligibilityCertificate(vrfSecret, leaderElectionInterpreter, eta, relativeStake, parent.slot)

      val (unsignedOriginal, linearSK) =
        withPartialOperationalCertificate(
          slot,
          partial =>
            UnsignedBlockHeader(
              parentHeaderId = parent.id,
              parentSlot = parent.slot,
              txRoot = txRoot.data,
              bloomFilter = bloomFilter.data,
              timestamp = System.currentTimeMillis(),
              height = parent.height + 1,
              slot = slot,
              eligibilityCertificate = eligibilityCertificate,
              partialOperationalCertificate = partial,
              metadata = ByteString.EMPTY,
              address = stakingAddress
            ),
          kesSK0
        )
      val unsigned = preSign(unsignedOriginal)

      val operationalCertificate =
        co.topl.consensus.models.OperationalCertificate(
          unsigned.partialOperationalCertificate.parentVK,
          unsigned.partialOperationalCertificate.parentSignature,
          unsigned.partialOperationalCertificate.childVK,
          ByteString.copyFrom(
            ed25519.sign(Ed25519.SecretKey(linearSK.toByteArray), unsigned.signableBytes.toByteArray)
          )
        )

      val child =
        BlockHeader(
          parentHeaderId = unsigned.parentHeaderId,
          parentSlot = unsigned.parentSlot,
          txRoot = unsigned.txRoot,
          bloomFilter = unsigned.bloomFilter,
          timestamp = unsigned.timestamp,
          height = unsigned.height,
          slot = unsigned.slot,
          eligibilityCertificate = unsigned.eligibilityCertificate,
          operationalCertificate = operationalCertificate,
          metadata = unsigned.metadata,
          address = unsigned.address
        )

      (parent, child, registration, eta, relativeStake)
    }

  private def simpleHeaderStore(id: BlockId, header: BlockHeader) = {
    val store = mock[Store[F, BlockId, BlockHeader]]
    (store
      .getOrRaise(_: BlockId)(_: MonadThrow[F], _: Show[BlockId]))
      .expects(id, *, *)
      .returning(header.pure[F])
    store
  }
}

object BlockHeaderValidationSpec {

  // Note: These methods are in the companion object because `digest.Digest32#value` conflicts with a ScalaTest member
  def validRegistrationNew(
    vkVrf:  ByteString,
    poolVK: StakingAddress,
    skKes:  SecretKeyKesProduct
  )(implicit kesProduct: KesProduct): SignatureKesProduct = {
    val commitmentMessage = new Blake2b256().hash(vkVrf.concat(poolVK.value))
    kesProduct.sign(skKes, commitmentMessage.toArray)
  }
}
