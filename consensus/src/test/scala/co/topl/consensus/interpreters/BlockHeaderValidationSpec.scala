package co.topl.consensus.interpreters

import cats.MonadThrow
import cats.Show
import cats.data.EitherT
import cats.effect._
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
import co.topl.interpreters.CatsUnsafeResource
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
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.Gen
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

import java.util.UUID
import scala.util.Random

class BlockHeaderValidationSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  import BlockHeaderValidationSpec._

  private val sharedBigBangBlockId = arbitraryBlockId.arbitrary.first

  private val sharedKesProductResource =
    ResourceSuiteLocalFixture("kes-product", CatsUnsafeResource.make[F, KesProduct](new KesProduct, 1).toResource)

  private val sharedEd25519Resource =
    ResourceSuiteLocalFixture("ed25519", CatsUnsafeResource.make[F, Ed25519](new Ed25519, 1).toResource)

  private val sharedEd25519VRFResource =
    ResourceSuiteLocalFixture(
      "ed25519-vrf",
      CatsUnsafeResource.make[F, Ed25519VRF](Ed25519VRF.precomputed(), 1).toResource
    )

  private val sharedBlake2b256Resource =
    ResourceSuiteLocalFixture("blake2b256", CatsUnsafeResource.make[F, Blake2b256](new Blake2b256, 1).toResource)

  private val sharedLeaderElectionInterpreter =
    ResourceSuiteLocalFixture(
      "leader-election",
      (
        CatsUnsafeResource.make[F, Blake2b512](new Blake2b512, 1).toResource,
        ExpInterpreter.make[F](10000, 38).toResource,
        Log1pInterpreter.make[F](10000, 16).toResource.evalMap(Log1pInterpreter.makeCached[F])
      )
        .mapN((blake2b512, exp, log1p) =>
          LeaderElectionValidation.make[F](
            VrfConfig(lddCutoff = 0, precision = 16, baselineDifficulty = Ratio(1, 15), amplitude = Ratio(2, 5)),
            blake2b512,
            exp,
            log1p
          )
        )
        .evalMap(LeaderElectionValidation.makeCached[F])
    )

  override def munitFixtures = List(
    sharedKesProductResource,
    sharedEd25519Resource,
    sharedEd25519VRFResource,
    sharedBlake2b256Resource,
    sharedLeaderElectionInterpreter
  )

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(3)
      .withMaxDiscardRatio(2)

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

  test("invalidate blocks with non-forward slot") {
    PropF.forAllF(genValid(u => u.copy(slot = 0L))) { case (parent, child, _: SignatureKesProduct, _: Eta, _: Ratio) =>
      withMock {
        val headerStore = simpleHeaderStore(child.parentHeaderId, parent)

        createValidation(headerStore = headerStore)
          .evalMap(
            validateErrorValue(_)(child)(
              BlockHeaderValidationFailures.NonForwardSlot(child.slot, parent.slot)
            )
          )
          .use_
      }
    }
  }

  test("invalidate blocks with non-forward timestamp") {
    PropF
      .forAllF(genValid(u => u.copy(timestamp = 0L))) {
        case (parent, child, _: SignatureKesProduct, _: Eta, _: Ratio) =>
          withMock {
            val headerStore = simpleHeaderStore(child.parentHeaderId, parent)

            createValidation(headerStore = headerStore)
              .evalMap(
                validateErrorValue(_)(child)(
                  BlockHeaderValidationFailures.NonForwardTimestamp(child.timestamp, parent.timestamp)
                )
              )
              .use_
          }
      }
  }

  test("invalidate blocks with incorrect timestamp for slot") {
    PropF
      .forAllF(for {
        slotError <- Gen.chooseNum[Long](Int.MinValue, Int.MaxValue).suchThat(_ != 0)
        (parent, child, _: SignatureKesProduct, _: Eta, _: Ratio) <- genValid()
      } yield (parent, child, slotError)) { case (parent, child, slotError) =>
        withMock {
          val headerStore = simpleHeaderStore(child.parentHeaderId, parent)
          val clockAlgebra = mock[ClockAlgebra[F]]
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

          createValidation(clock = clockAlgebra, headerStore = headerStore)
            .evalMap(
              validateErrorValue(_)(child)(
                BlockHeaderValidationFailures.TimestampSlotMismatch(child.slot, child.timestamp)
              )
            )
            .use_
        }
      }
  }

  // TODO do we need to care about long overflow if (forwardWindowBias + global slot) > Long.MaxValue?
  test("invalidate block if slot is greater than current global slot + forward biased slot window") {
    PropF
      .forAllF(for {
        globalSlot        <- Gen.chooseNum[Long](1, Int.MaxValue)
        parentSlot        <- Gen.const[Long](globalSlot - 1)
        forwardSlotWindow <- Gen.chooseNum[Long](1, Int.MaxValue)
        biasValue         <- Gen.chooseNum[Long](1, Int.MaxValue)
        slotFromFuture    <- Gen.chooseNum[Long](globalSlot + forwardSlotWindow + biasValue, Long.MaxValue)
        (parent, child, _: SignatureKesProduct, _: Eta, _: Ratio) <- genValid(
          childHeader => childHeader.copy(slot = slotFromFuture),
          parentSlot = parentSlot
        )
      } yield (parent, child, forwardSlotWindow, globalSlot)) { case (parent, child, forwardWindowBias, globalSlot) =>
        withMock {
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

          createValidation(clock = clockAlgebra, headerStore = headerStore)
            .evalMap(
              validateErrorValue(_)(child)(
                BlockHeaderValidationFailures.SlotBeyondForwardBiasedSlotWindow(globalSlot, child.slot)
              )
            )
            .use_
        }
      }
  }

  test("invalidate blocks with syntactically incorrect VRF certificate for a particular nonce") {
    PropF.forAllF(
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
      co.topl.models.ModelGenerators.etaGen
    ) { case ((parent, child), eta) =>
      withMock {
        val etaInterpreter = mock[EtaCalculationAlgebra[F]]
        val clockAlgebra = createDummyClockAlgebra(child)
        val headerStore = simpleHeaderStore(child.parentHeaderId, parent)

        (etaInterpreter
          .etaToBe(_: SlotId, _: Slot))
          .expects(SlotId(parent.slot, parent.id), child.slot)
          .anyNumberOfTimes()
          // This epoch nonce does not satisfy the generated VRF certificate
          .returning(eta.pure[F])

        createValidation(
          clock = clockAlgebra,
          headerStore = headerStore,
          leaderElectionInterpreter = sharedLeaderElectionInterpreter(),
          ed25519VRFResource = sharedEd25519VRFResource(),
          kesProductResource = sharedKesProductResource(),
          ed25519Resource = sharedEd25519Resource(),
          blake2b256Resource = sharedBlake2b256Resource(),
          etaInterpreter = etaInterpreter
        )
          .evalMap(
            validateErrorType[BlockHeaderValidationFailures.InvalidEligibilityCertificateEta](_)(child)
          )
          .use_
      }
    }
  }

  test("invalidate blocks with a syntactically incorrect KES certificate") {
    PropF
      .forAllF(genValid()) { case (parent, child, _: SignatureKesProduct, eta, _: Ratio) =>
        withMock {
          val etaInterpreter = mock[EtaCalculationAlgebra[F]]
          val clockAlgebra = createDummyClockAlgebra(child)
          val headerStore = simpleHeaderStore(child.parentHeaderId, parent)

          // Changing any bytes of the block will result in a bad block signature
          val badBlock = child.copy(timestamp = child.timestamp + 1)

          (etaInterpreter
            .etaToBe(_: SlotId, _: Slot))
            .expects(SlotId(parent.slot, parent.id), badBlock.slot)
            .anyNumberOfTimes()
            .returning(eta.pure[F])

          createValidation(
            clock = clockAlgebra,
            headerStore = headerStore,
            leaderElectionInterpreter = sharedLeaderElectionInterpreter(),
            ed25519VRFResource = sharedEd25519VRFResource(),
            kesProductResource = sharedKesProductResource(),
            ed25519Resource = sharedEd25519Resource(),
            blake2b256Resource = sharedBlake2b256Resource(),
            etaInterpreter = etaInterpreter
          )
            .evalMap(
              validateErrorType[BlockHeaderValidationFailures.InvalidBlockProof](_)(badBlock)
            )
            .use_
        }
      }
  }

  test("invalidate blocks with a semantically incorrect registration verification") {
    PropF
      .forAllF(
        genValid(u =>
          u.copy(
            address = StakingAddress(ByteString.copyFrom(Array.fill[Byte](32)(0)))
          )
        )
      ) { case (parent, child, registration, eta, _: Ratio) =>
        withMock {
          val consensusValidationState = mock[ConsensusValidationStateAlgebra[F]]
          val etaInterpreter = mock[EtaCalculationAlgebra[F]]
          val clockAlgebra = createDummyClockAlgebra(child)
          val headerStore = simpleHeaderStore(child.parentHeaderId, parent)

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
          createValidation(
            clock = clockAlgebra,
            headerStore = headerStore,
            consensusValidationState = consensusValidationState,
            leaderElectionInterpreter = sharedLeaderElectionInterpreter(),
            ed25519VRFResource = sharedEd25519VRFResource(),
            kesProductResource = sharedKesProductResource(),
            ed25519Resource = sharedEd25519Resource(),
            blake2b256Resource = sharedBlake2b256Resource(),
            etaInterpreter = etaInterpreter
          )
            .evalMap(
              validateErrorType[BlockHeaderValidationFailures.RegistrationCommitmentMismatch](_)(child)
            )
            .use_
        }
      }
  }

  test("invalidate blocks with an insufficient VRF threshold") {
    PropF
      .forAllF(genValid()) { case (parent, child, registration, eta, _: Ratio) =>
        withMock {
          val consensusValidationState = mock[ConsensusValidationStateAlgebra[F]]
          val etaInterpreter = mock[EtaCalculationAlgebra[F]]
          val clockAlgebra = createDummyClockAlgebra(child)
          val headerStore = simpleHeaderStore(child.parentHeaderId, parent)

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

          createValidation(
            clock = clockAlgebra,
            headerStore = headerStore,
            consensusValidationState = consensusValidationState,
            leaderElectionInterpreter = sharedLeaderElectionInterpreter(),
            ed25519VRFResource = sharedEd25519VRFResource(),
            kesProductResource = sharedKesProductResource(),
            ed25519Resource = sharedEd25519Resource(),
            blake2b256Resource = sharedBlake2b256Resource(),
            etaInterpreter = etaInterpreter
          )
            .evalMap(
              validateErrorType[BlockHeaderValidationFailures.InvalidVrfThreshold](_)(child)
            )
            .use_
        }
      }
  }

  test("invalidate blocks with recently claimed eligibilities") {
    PropF
      .forAllF(genValid()) { case (parent, child, registration, eta, relativeStake) =>
        withMock {
          val consensusValidationState = mock[ConsensusValidationStateAlgebra[F]]
          val etaInterpreter = mock[EtaCalculationAlgebra[F]]
          val clockAlgebra = createDummyClockAlgebra(child)
          val headerStore = simpleHeaderStore(child.parentHeaderId, parent)
          val eligibilityCache = mock[EligibilityCacheAlgebra[F]]

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

          (eligibilityCache
            .tryInclude(_: BlockId, _: Bytes, _: Slot))
            .expects(*, *, *)
            .once()
            .returning(false.pure[F])

          createValidation(
            clock = clockAlgebra,
            headerStore = headerStore,
            consensusValidationState = consensusValidationState,
            leaderElectionInterpreter = sharedLeaderElectionInterpreter(),
            eligibilityCache = eligibilityCache,
            ed25519VRFResource = sharedEd25519VRFResource(),
            kesProductResource = sharedKesProductResource(),
            ed25519Resource = sharedEd25519Resource(),
            blake2b256Resource = sharedBlake2b256Resource(),
            etaInterpreter = etaInterpreter
          )
            .evalMap(
              validateErrorType[BlockHeaderValidationFailures.DuplicateEligibility](_)(child)
            )
            .use_
        }
      }
  }

  // TODO: Fix this test, talk with Sean
  test("validate valid blocks") {
    PropF
      .forAllF(genValid()) { case (parent, child, registration, eta, relativeStake) =>
        withMock {
          val consensusValidationState = mock[ConsensusValidationStateAlgebra[F]]
          val etaInterpreter = mock[EtaCalculationAlgebra[F]]
          val clockAlgebra = createDummyClockAlgebra(child)
          val headerStore = simpleHeaderStore(child.parentHeaderId, parent)
          val eligibilityCache = mock[EligibilityCacheAlgebra[F]]

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

          (eligibilityCache
            .tryInclude(_: BlockId, _: Bytes, _: Slot))
            .expects(*, *, *)
            .anyNumberOfTimes()
            .returning(true.pure[F])

          createValidation(
            clock = clockAlgebra,
            headerStore = headerStore,
            consensusValidationState = consensusValidationState,
            leaderElectionInterpreter = sharedLeaderElectionInterpreter(),
            eligibilityCache = eligibilityCache,
            ed25519VRFResource = sharedEd25519VRFResource(),
            kesProductResource = sharedKesProductResource(),
            ed25519Resource = sharedEd25519Resource(),
            blake2b256Resource = sharedBlake2b256Resource(),
            etaInterpreter = etaInterpreter
          )
            .evalMap(underTest => underTest.validate(child).assertEquals(Right(child)))
            .use_
        }
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
        Ed25519VRF.precomputed().sign(skVrf.toByteArray, VrfArgument(eta, slot).signableBytes.toByteArray)
      )

    def isLeader(threshold: Ratio, testProof: ByteString) =
      thresholdInterpreter
        .isSlotLeaderForThreshold(threshold)(
          Rho(Sized.strictUnsafe(ByteString.copyFrom(Ed25519VRF.precomputed().proofToHash(testProof.toByteArray))))
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
      ByteString.copyFrom(Ed25519VRF.precomputed().getVerificationKey(skVrf.toByteArray)),
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
    val kesProduct = new KesProduct()
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
      val (kesSK0, _) = new KesProduct().createKeyPair(Random.nextBytes(32), (9, 9), 0L)
      val poolVK = ByteString.copyFrom(Random.nextBytes(32))
      val stakingAddress = StakingAddress(poolVK)
      val vrfSecret = ByteString.copyFrom(vrfSecretBytes)

      val registration = validRegistrationNew(
        ByteString.copyFrom(Ed25519VRF.precomputed().getVerificationKey(vrfSecretBytes)),
        stakingAddress,
        kesSK0
      )(new KesProduct)

      val (eligibilityCertificate, slot) =
        validEligibilityCertificate(vrfSecret, sharedLeaderElectionInterpreter(), eta, relativeStake, parent.slot)

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
            new Ed25519().sign(Ed25519.SecretKey(linearSK.toByteArray), unsigned.signableBytes.toByteArray)
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

  private def createValidation(
    etaInterpreter:            EtaCalculationAlgebra[F] = mock[EtaCalculationAlgebra[F]],
    consensusValidationState:  ConsensusValidationStateAlgebra[F] = mock[ConsensusValidationStateAlgebra[F]],
    leaderElectionInterpreter: LeaderElectionValidationAlgebra[F] = mock[LeaderElectionValidationAlgebra[F]],
    eligibilityCache:          EligibilityCacheAlgebra[F] = mock[EligibilityCacheAlgebra[F]],
    clock:                     ClockAlgebra[F] = mock[ClockAlgebra[F]],
    headerStore:               Store[F, BlockId, BlockHeader] = mock[Store[F, BlockId, BlockHeader]],
    ed25519VRFResource:        UnsafeResource[F, Ed25519VRF] = mock[UnsafeResource[F, Ed25519VRF]],
    kesProductResource:        UnsafeResource[F, KesProduct] = mock[UnsafeResource[F, KesProduct]],
    ed25519Resource:           UnsafeResource[F, Ed25519] = mock[UnsafeResource[F, Ed25519]],
    blake2b256Resource:        UnsafeResource[F, Blake2b256] = mock[UnsafeResource[F, Blake2b256]]
  ): Resource[F, BlockHeaderValidationAlgebra[F]] =
    BlockHeaderValidation
      .make[F](
        etaInterpreter,
        consensusValidationState,
        leaderElectionInterpreter,
        eligibilityCache,
        clock,
        headerStore,
        sharedBigBangBlockId,
        ed25519VRFResource,
        kesProductResource,
        ed25519Resource,
        blake2b256Resource
      )
      .toResource

  private def validateErrorType[E](underTest: BlockHeaderValidationAlgebra[F])(header: BlockHeader) =
    EitherT(underTest.validate(header)).swap
      .getOrRaise(new IllegalStateException("Validation unexpectedly succeeded"))
      .map(_.isInstanceOf[E @unchecked])
      .assert
      .void

  private def validateErrorValue(
    underTest: BlockHeaderValidationAlgebra[F]
  )(header: BlockHeader)(expectedFailure: BlockHeaderValidationFailure) =
    EitherT(underTest.validate(header)).swap
      .getOrRaise(new IllegalStateException("Validation unexpectedly succeeded"))
      .map(_ == expectedFailure)
      .assert
      .void
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
