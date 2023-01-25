package co.topl.models.generators.models

import co.topl.proto.models._
import co.topl.models.generators.common.ModelGenerators._
import co.topl.models.utility.{Lengths, Sized}
import com.google.protobuf.ByteString
import org.scalacheck.{Arbitrary, Gen}

/**
 * Model Generators related to: https://github.com/Topl/protobuf-specs/blob/main/models
 */
trait ModelGenerators {

  def byteGen: Gen[Byte] = Gen.choose[Byte](Byte.MinValue, Byte.MaxValue)

  implicit val arbitraryUnknownFieldSet: Arbitrary[scalapb.UnknownFieldSet] =
    Arbitrary(Gen.const(scalapb.UnknownFieldSet.empty))

  val arbitraryPositiveInt128: Arbitrary[Int128] =
    Arbitrary(
      for {
        value <- arbitraryByteString.arbitrary
      } yield Int128(value, scalapb.UnknownFieldSet.empty)
    )

  implicit val arbitraryTransactionId: Arbitrary[TransactionId] =
    Arbitrary(
      for {
        value <- arbitraryByteString.arbitrary
      } yield TransactionId.of(value)
    )

  implicit val arbitraryBoxId: Arbitrary[Box.Id] =
    Arbitrary(
      for {
        transactionId          <- arbitraryTransactionId.arbitrary
        transactionOutputIndex <- Gen.posNum[Short]
      } yield Box.Id.of(Some(transactionId), transactionOutputIndex)
    )

  implicit val arbitraryBox: Arbitrary[Box] =
    Arbitrary(
      for {
        evidence <- typedEvidenceGen
        value    <- arbitraryBoxValue.arbitrary
      } yield Box.of(Some(evidence), value)
    )

  implicit val arbitraryBoxLocation: Arbitrary[BoxLocation] =
    Arbitrary(
      for {
        index    <- Gen.posNum[Short]
        location <- Gen.oneOf(BoxLocation.IO.INPUT, BoxLocation.IO.OUTPUT)
      } yield BoxLocation.of(index, location)
    )

  implicit val arbitraryCurve25519VK: Arbitrary[VerificationKeyCurve25519] =
    Arbitrary(
      for {
        value <- arbitraryByteString.arbitrary
      } yield VerificationKeyCurve25519.of(value)
    )

  implicit val arbitraryPropositionsPermanentlyLocked: Arbitrary[PropositionPermanentlyLocked] =
    Arbitrary(Gen.const(PropositionPermanentlyLocked.of()))

  implicit val arbitraryPropositionKnowledgeCurve25519: Arbitrary[PropositionKnowledgeCurve25519] =
    Arbitrary(
      for {
        vk <- arbitraryCurve25519VK.arbitrary
      } yield PropositionKnowledgeCurve25519.of(Some(vk))
    )

  implicit val arbitraryPropositionKnowledgeEd25519: Arbitrary[PropositionKnowledgeEd25519] =
    Arbitrary(
      for {
        vk <- verificationKeyEd25519Gen
      } yield PropositionKnowledgeEd25519.of(Some(vk))
    )

  implicit val arbitraryPropositionKnowledgeExtendedEd25519: Arbitrary[PropositionKnowledgeExtendedEd25519] =
    Arbitrary(
      for {
        vk <- verificationKeyExtendedEd25519Gen
      } yield PropositionKnowledgeExtendedEd25519.of(Some(vk))
    )

  implicit val arbitraryPropositionKnowledgeHashLock: Arbitrary[PropositionKnowledgeHashLock] =
    Arbitrary(
      for {
        valueDigest <- arbitraryByteString.arbitrary
      } yield PropositionKnowledgeHashLock.of(valueDigest)
    )

  implicit val arbitraryPropositionCompositionalThreshold: Arbitrary[PropositionCompositionalThreshold] =
    Arbitrary(
      for {
        threshold <- Gen.chooseNum[Int](1, 5)
        propositions <- Gen
          .containerOfN[Seq, Proposition](threshold + 1, Gen.delay(arbitraryProposition.arbitrary))

      } yield PropositionCompositionalThreshold.of(threshold, propositions)
    )

  implicit val arbitraryPropositionCompositionalAnd: Arbitrary[PropositionCompositionalAnd] =
    Arbitrary(
      for {
        a <- Gen.delay(arbitraryProposition.arbitrary)
        b <- Gen.delay(arbitraryProposition.arbitrary)
      } yield PropositionCompositionalAnd.of(a, b)
    )

  implicit val arbitraryPropositionCompositionalOr: Arbitrary[PropositionCompositionalOr] =
    Arbitrary(
      for {
        a <- Gen.delay(arbitraryProposition.arbitrary)
        b <- Gen.delay(arbitraryProposition.arbitrary)
      } yield PropositionCompositionalOr.of(a, b)
    )

  implicit val arbitraryPropositionCompositionalNot: Arbitrary[PropositionCompositionalNot] =
    Arbitrary(
      for {
        a <- Gen.delay(arbitraryProposition.arbitrary)
      } yield PropositionCompositionalNot.of(a)
    )

  implicit val arbitraryPropositionContextualHeightLock: Arbitrary[PropositionContextualHeightLock] =
    Arbitrary(
      for {
        height <- Gen.posNum[Long]
      } yield PropositionContextualHeightLock.of(height)
    )

  implicit val arbitraryPropositionContextualRequiredTransactionIORequirement
    : Arbitrary[PropositionContextualRequiredTransactionIO.Requirement] =
    Arbitrary(
      for {
        box      <- arbitraryBox.arbitrary
        location <- arbitraryBoxLocation.arbitrary
      } yield PropositionContextualRequiredTransactionIO.Requirement.of(Some(box), Some(location))
    )

  implicit val arbitraryPropositionContextualRequiredTransactionIO
    : Arbitrary[PropositionContextualRequiredTransactionIO] =
    Arbitrary(
      for {
        requirements <-
          Gen.containerOfN[Seq, PropositionContextualRequiredTransactionIO.Requirement](
            3,
            arbitraryPropositionContextualRequiredTransactionIORequirement.arbitrary
          )
      } yield PropositionContextualRequiredTransactionIO.of(requirements)
    )

  implicit val arbitraryProposition: Arbitrary[Proposition] =
    Arbitrary(
      Gen.oneOf(
        implicitly[Arbitrary[PropositionPermanentlyLocked]].arbitrary,
        implicitly[Arbitrary[PropositionKnowledgeCurve25519]].arbitrary,
        implicitly[Arbitrary[PropositionKnowledgeEd25519]].arbitrary,
        implicitly[Arbitrary[PropositionKnowledgeExtendedEd25519]].arbitrary,
        implicitly[Arbitrary[PropositionKnowledgeHashLock]].arbitrary,
        implicitly[Arbitrary[PropositionCompositionalThreshold]].arbitrary,
        implicitly[Arbitrary[PropositionCompositionalAnd]].arbitrary,
        implicitly[Arbitrary[PropositionCompositionalOr]].arbitrary,
        implicitly[Arbitrary[PropositionCompositionalNot]].arbitrary,
        implicitly[Arbitrary[PropositionContextualHeightLock]].arbitrary,
        implicitly[Arbitrary[PropositionContextualRequiredTransactionIO]].arbitrary
      )
    )

  implicit val arbitraryProofUndefined: Arbitrary[ProofUndefined] =
    Arbitrary(Gen.const(ProofUndefined.of()))

  implicit val arbitraryProofKnowledgeCurve25519: Arbitrary[ProofKnowledgeCurve25519] =
    Arbitrary(
      for {
        value <- arbitraryByteString.arbitrary
      } yield ProofKnowledgeCurve25519.of(value)
    )

  implicit val arbitraryProofKnowledgeEd25519: Arbitrary[ProofKnowledgeEd25519] =
    Arbitrary(
      for {
        value <- arbitraryByteString.arbitrary
      } yield ProofKnowledgeEd25519.of(value)
    )

  implicit val arbitraryProofKnowledgeVrfEd25519: Arbitrary[ProofKnowledgeVrfEd25519] =
    Arbitrary(
      for {
        value <- arbitraryByteString.arbitrary
      } yield ProofKnowledgeVrfEd25519.of(value)
    )

  implicit val arbitraryProofKnowledgeKesSum: Arbitrary[ProofKnowledgeKesSum] =
    Arbitrary(
      for {
        verificationKey <- verificationKeyEd25519Gen
        signature       <- arbitraryProofKnowledgeEd25519.arbitrary
        witness <- Gen.nonEmptyContainerOf[Seq, Sized.Strict[ByteString, Lengths.`32`.type]](
          genSizedStrictByteString[Lengths.`32`.type]()
        )

      } yield ProofKnowledgeKesSum.of(Some(verificationKey), Some(signature), witness.map(_.data))
    )

  implicit val arbitraryProofKnowledgeKesProduct: Arbitrary[ProofKnowledgeKesProduct] =
    Arbitrary(
      for {
        superSignature <- arbitraryProofKnowledgeKesSum.arbitrary
        subSignature   <- arbitraryProofKnowledgeKesSum.arbitrary
        subRoot        <- arbitraryByteString.arbitrary
      } yield ProofKnowledgeKesProduct.of(Some(superSignature), Some(subSignature), subRoot)
    )

  implicit val arbitraryProofKnowledgeHashLock: Arbitrary[ProofKnowledgeHashLock] =
    Arbitrary(
      for {
        value <- arbitraryByteString.arbitrary
      } yield ProofKnowledgeHashLock.of(value)
    )

  implicit val arbitraryProofCompositionalThreshold: Arbitrary[ProofCompositionalThreshold] =
    Arbitrary(
      for {
        proofs <- Gen.containerOfN[Seq, Proof](3, Gen.delay(arbitraryProof.arbitrary))
      } yield ProofCompositionalThreshold.of(proofs)
    )

  implicit val arbitraryProofCompositionalAnd: Arbitrary[ProofCompositionalAnd] =
    Arbitrary(
      for {
        a <- Gen.delay(arbitraryProof.arbitrary)
        b <- Gen.delay(arbitraryProof.arbitrary)
      } yield ProofCompositionalAnd.of(a, b)
    )

  implicit val arbitraryProofCompositionalOr: Arbitrary[ProofCompositionalOr] =
    Arbitrary(
      for {
        a <- Gen.delay(arbitraryProof.arbitrary)
        b <- Gen.delay(arbitraryProof.arbitrary)
      } yield ProofCompositionalOr.of(a, b)
    )

  implicit val arbitraryProofCompositionalNot: Arbitrary[ProofCompositionalNot] =
    Arbitrary(
      for {
        a <- Gen.delay(arbitraryProof.arbitrary)
      } yield ProofCompositionalNot.of(a)
    )

  implicit val arbitraryProofContextualHeightLock: Arbitrary[ProofContextualHeightLock] =
    Arbitrary(Gen.const(ProofContextualHeightLock.of()))

  implicit val arbitraryProofContextualRequiredTransactionIO: Arbitrary[ProofContextualRequiredTransactionIO] =
    Arbitrary(Gen.const(ProofContextualRequiredTransactionIO.of()))

  implicit val arbitraryProof: Arbitrary[Proof] =
    Arbitrary(
      Gen.oneOf(
        arbitraryProofUndefined.arbitrary,
        arbitraryProofKnowledgeCurve25519.arbitrary,
        arbitraryProofKnowledgeEd25519.arbitrary,
        arbitraryProofKnowledgeVrfEd25519.arbitrary,
        arbitraryProofKnowledgeKesSum.arbitrary,
        arbitraryProofKnowledgeKesProduct.arbitrary,
        arbitraryProofKnowledgeHashLock.arbitrary,
        arbitraryProofCompositionalThreshold.arbitrary,
        arbitraryProofCompositionalAnd.arbitrary,
        arbitraryProofCompositionalOr.arbitrary,
        arbitraryProofCompositionalNot.arbitrary,
        arbitraryProofContextualHeightLock.arbitrary,
        arbitraryProofContextualRequiredTransactionIO.arbitrary
      )
    )

  implicit val arbitraryPolyBoxValue: Arbitrary[PolyBoxValue] =
    Arbitrary(arbitraryPositiveInt128.arbitrary.map(int128 => PolyBoxValue.of(Some(int128))))

  implicit val arbitraryArbitBoxValue: Arbitrary[ArbitBoxValue] =
    Arbitrary(arbitraryPositiveInt128.arbitrary.map(int128 => ArbitBoxValue.of(Some(int128))))

  implicit val arbitraryAssetV1BoxValueCode: Arbitrary[AssetV1BoxValue.Code] =
    Arbitrary(
      for {
        issuerAddress <- arbitrarySpendingAddress.arbitrary
        shortName     <- arbitraryByteString.arbitrary
      } yield AssetV1BoxValue.Code(Some(issuerAddress), shortName)
    )

  implicit val arbitraryAssetV1BoxValue: Arbitrary[AssetV1BoxValue] =
    Arbitrary(
      for {
        int128       <- arbitraryPositiveInt128.arbitrary
        securityRoot <- arbitraryByteString.arbitrary
        assetCode    <- arbitraryAssetV1BoxValueCode.arbitrary
        metadata     <- arbitraryByteString.arbitrary
      } yield AssetV1BoxValue.of(Some(int128), Some(assetCode), securityRoot, metadata)
    )

  implicit val arbitraryOperatorRegistrationBoxValue: Arbitrary[OperatorRegistrationBoxValue] =
    Arbitrary(
      for {
        vrfCommitment <- arbitraryProofKnowledgeKesProduct.arbitrary
      } yield OperatorRegistrationBoxValue.of(Some(vrfCommitment))
    )

  def arbitraryBoxValue: Arbitrary[BoxValue] =
    Arbitrary(
      Gen.oneOf(
        Gen.const(EmptyBoxValue.of()),
        arbitraryPolyBoxValue.arbitrary,
        arbitraryArbitBoxValue.arbitrary,
        arbitraryAssetV1BoxValue.arbitrary,
        arbitraryOperatorRegistrationBoxValue.arbitrary
      )
    )

  def networkPrefixGen: Gen[NetworkPrefix] =
    byteGen.map(NetworkPrefix.of(_))

  def typedEvidenceGen: Gen[TypedEvidence] =
    for {
      prefix   <- byteGen
      evidence <- arbitraryByteString.arbitrary
    } yield TypedEvidence.of(prefix, evidence)

  implicit def arbitrarySpendingAddress: Arbitrary[SpendingAddress] =
    Arbitrary(
      for {
        typedEvidence <- typedEvidenceGen
      } yield SpendingAddress.of(Some(typedEvidence))
    )

  def verificationKeyEd25519Gen: Gen[VerificationKeyEd25519] =
    for {
      value <- arbitraryByteString.arbitrary
    } yield VerificationKeyEd25519.of(value)

  def verificationKeyExtendedEd25519Gen: Gen[VerificationKeyExtendedEd25519] =
    for {
      vk        <- verificationKeyEd25519Gen
      chainCode <- arbitraryByteString.arbitrary
    } yield VerificationKeyExtendedEd25519.of(Some(vk), chainCode)

  implicit val proofsKnowledgeEd25519: Gen[ProofKnowledgeEd25519] =
    for {
      value <- arbitraryByteString.arbitrary
    } yield ProofKnowledgeEd25519.of(value)

  def operatorStakingAddressGen: Gen[StakingAddressOperator] =
    for {
      poolVK <- verificationKeyEd25519Gen
    } yield StakingAddressOperator.of(Some(poolVK))

  def stakingAddressGen: Gen[StakingAddress] =
    operatorStakingAddressGen

  implicit val arbitraryFullAddress: Arbitrary[FullAddress] =
    Arbitrary(
      for {
        prefix   <- networkPrefixGen
        spending <- arbitrarySpendingAddress.arbitrary
        staking  <- stakingAddressGen
        binding  <- proofsKnowledgeEd25519
      } yield FullAddress.of(Some(prefix), Some(spending), staking, Some(binding))
    )

  implicit val arbitraryTransactionInput: Arbitrary[Transaction.Input] =
    Arbitrary(
      for {
        boxId       <- arbitraryBoxId.arbitrary
        proposition <- arbitraryProposition.arbitrary
        proof       <- arbitraryProof.arbitrary
        value       <- arbitraryBoxValue.arbitrary
      } yield Transaction.Input.of(Some(boxId), proposition, proof, value)
    )

  implicit val arbitraryTransactionOutput: Arbitrary[Transaction.UnspentOutput] =
    Arbitrary(
      for {
        address  <- arbitraryFullAddress.arbitrary
        value    <- arbitraryBoxValue.arbitrary
        minting  <- Gen.prob(0.05)
        metadata <- arbitraryByteString.arbitrary
      } yield Transaction.UnspentOutput.of(Some(address), value, minting, metadata)
    )

  implicit val arbitraryTransactionSchedule: Arbitrary[Transaction.Schedule] =
    Arbitrary(
      for {
        creation    <- Gen.chooseNum[Long](0L, 100000L)
        minimumSlot <- Gen.chooseNum[Long](0L, 50000L)
        maximumSlot <- Gen.chooseNum[Long](minimumSlot, 100000L)
      } yield Transaction.Schedule.of(creation, minimumSlot, maximumSlot)
    )

  implicit val arbitraryTransaction: Arbitrary[Transaction] =
    Arbitrary(
      for {
        inputs <-
          Gen
            .chooseNum[Int](1, 4)
            .flatMap(count =>
              Gen
                .listOfN(count, arbitraryTransactionInput.arbitrary)
            )
        outputs <-
          Gen
            .chooseNum[Int](1, 4)
            .flatMap(count =>
              Gen
                .listOfN(count, arbitraryTransactionOutput.arbitrary)
            )
        schedule <- arbitraryTransactionSchedule.arbitrary
        data     <- arbitraryByteString.arbitrary
      } yield Transaction.of(inputs, outputs, Some(schedule), data)
    )

}
object ModelGenerators extends ModelGenerators
